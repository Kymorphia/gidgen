module gir.report;

import defs;
import gir.base;
import gir.field;
import gir.func;
import gir.repo;
import gir.structure;
import std_includes;

/// Class used for generating binding coverage reports
class Report
{
  this(Defs defs)
  {
    foreach (repo; defs.repos)
    {
      auto report = RepoReport(repo);

      foreach (st; repo.structs) // Loop over structures
      { // Add the structure to the report objects array by active state
        report.objs[cast(int)st.active][cast(int)ReportObjType.Structs] ~= st;

        foreach (ndx, objArray; tuple(st.functions, st.signals, st.fields)) // Add functions, signals, and fields by active state
          foreach (obj; objArray)
            report.objs[cast(int)obj.active][ndx + 1] ~= obj;
      }

      repoReports ~= report;
    }
  }

  string generate(ReportFlags flags)
  {
    enum COUNT_LENGTH = 4; // Length to use for object counts (9999 is max)
    enum PERCENT_LENGTH = 5; // Length to use for percent (99.9%, 100% is used for 100)
    enum STATS_LENGTH = (COUNT_LENGTH + 1) * 3 + PERCENT_LENGTH;

    string reportStr = "\n[Report]\n";

    if (flags & ReportFlags.Summary)
    {
      auto maxRepoName = max(repoReports.map!(x => x.repo.dubPackageName.length).fold!max, "Package".length);

      // Summary header

      reportStr ~= "\n[Summary]\n";
      reportStr ~= format("%-*s ", maxRepoName, "Package");

      auto subHeader = ' '.repeat(maxRepoName + 1).array; // Sub header indicates which each of the count fields are (active, disabled, unsupported, percent covered)

      with (ReportFlags) // Add obj type headers for each flag specified
        foreach (f; tuple(Structs, Funcs, Signals, Fields))
          if (flags & f)
          {
            auto title = f.to!string;
            auto prependSpaces = (STATS_LENGTH - title.length) / 2;
            reportStr ~= "| " ~ ' '.repeat(prependSpaces).array ~ title
              ~ ' '.repeat(STATS_LENGTH - prependSpaces - title.length + 1).array;
          }

      subHeader ~= "|  Act  Dis  Uns  Perc ".repeat(reportStr.filter!(x => x == '|').walkLength).join;
      reportStr ~= "|\n" ~ subHeader ~ "|\n";

      foreach (report; repoReports)
      {
        reportStr ~= format("%-*s ", maxRepoName, report.repo.dubPackageName);

        with (ReportFlags) // Add obj type headers for each flag specified
        {
          foreach (objNdx, f; tuple(Structs, Funcs, Signals, Fields))
          {
            if (!(flags & f))
              continue;

            auto total = report.objs[cast(int)Active.Enabled][objNdx].length
              + report.objs[cast(int)Active.Unsupported][objNdx].length; // Don't include explicitly disabled objects in total
            auto perc = (total > 0 && report.objs[cast(int)Active.Enabled][objNdx].length < total) // Calculate coverage percentage
              ? format("%.1f", (report.objs[cast(int)Active.Enabled][objNdx].length * 100.0) / total) : "100";

            reportStr ~= format("| %*s %*s %*s %*s ", COUNT_LENGTH, report.objs[cast(int)Active.Enabled][objNdx].length,
              COUNT_LENGTH, report.objs[cast(int)Active.Disabled][objNdx].length,
              COUNT_LENGTH, report.objs[cast(int)Active.Unsupported][objNdx].length, PERCENT_LENGTH, perc);
          }
        }

        reportStr ~= "|\n";
      }
    }

    with (ReportFlags)
    {
      foreach (actNdx, af; tuple(Enabled, Disabled, Ignored, Unsupported))
      {
        if (!(flags & af))
          continue;

        foreach (objNdx, of; tuple(Structs, Funcs, Signals, Fields))
        {
          if (!(flags & of))
            continue;

          string[] fullNames;

          foreach (report; repoReports)
            fullNames ~= report.objs[actNdx][objNdx].map!(x => x.fullName.to!string).array;

          if (fullNames)
          {
            reportStr ~= "\n[" ~ af.to!string ~ " " ~ of.to!string ~ "]\n";
            reportStr ~= fullNames.join("\n") ~ "\n";
          }
        }
      }
    }

    return reportStr;
  }

private:
  RepoReport[] repoReports;
}

/// Report data for a repository
struct RepoReport
{
  Repo repo; /// The repo
  Base[][ReportObjType.max + 1][Active.max + 1] objs; /// All objects by type by active state
}

/// The different report object types
enum ReportObjType
{
  Structs,
  Funcs,
  Signals,
  Fields,
}

/// Report customization flags
enum ReportFlags
{
  Summary = 1 << 0, /// Output a summary table with stats

  Enabled = 1 << 1, /// Output Active.Enabled object names
  Disabled = 1 << 2, /// Output Active.Disabled object names
  Ignored = 1 << 3, /// Output Active.Ignored object names
  Unsupported = 1 << 4, /// Output Active.Unsupported object names

  Structs = 1 << 5, /// Output structures
  Funcs = 1 << 6, /// Output funcs
  Signals = 1 << 7, /// Output signals
  Fields = 1 << 8, /// Output fields

  All = Summary | Enabled | Disabled | Unsupported | Structs | Funcs | Signals | Fields, /// Output full report
  AllEnabled = Summary | Enabled | Structs | Funcs | Signals | Fields, /// Output report with enabled object names only
  AllDisabled = Summary | Disabled | Structs | Funcs | Signals | Fields, /// Output report with explicitly disabled object names only
  AllIgnored = Summary | Ignored | Structs | Funcs | Signals | Fields, /// Output report with ignored object names only
  AllUnsupported = Summary | Unsupported | Structs | Funcs | Signals | Fields, /// Output report with unsupported object names only
}

string[] reportFlagDescriptions = [
  "Generate a binding coverage summary table", // Summary
  "Show Enabled item names", // Enabled
  "Show Disabled item names", // Disabled
  "Show Ignored item names", // Ignored
  "Show Unsupported item names", // Unsupported
  "Show Structure/Class names", // Structs
  "Show Function/Method names", // Funcs
  "Show Signal names", // Signals
  "Show Field names", // Fields
  "Generate a full report", // All
  "Generate a report of all Enabled items", // AllEnabled
  "Generate a report of all Disabled items", // AllDisabled
  "Generate a report of all Ignored items", // AllIgnored
  "Generate a report of all Unsupported items (default)", // AllUnsupported
];

void displayReportOptionsHelp()
{
  writeln("Report options help (values can be logically OR'd with '|'):");

  auto names = [EnumMembers!ReportFlags].map!(x => x.to!string);
  auto maxNameLength = names.map!(x => x.length).fold!max;
  names.enumerate.map!(t => format("%-*s %s", maxNameLength, t.value, reportFlagDescriptions[t.index])).join("\n").writeln;
}

/**
 * Process a '|' delimited report flags option string to ReportFlags value.
 * Returns: A numeric ReportFlags value.
 * Throws: ConvException on error
 */
ReportFlags processReportOptions(string flagStr)
{
  return flagStr.split("|").map!(x => x.strip).filter!(x => !x.empty).map!(x => x.to!ReportFlags)
    .fold!((a, b) => a | b);
}
