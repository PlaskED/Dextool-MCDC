/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/

module dextool.plugin.cppmcdc.frontend.raw_args;

import logger = std.experimental.logger;

struct RawConfiguration {
    import std.getopt : GetoptResult, getopt, defaultGetoptPrinter;

    string[] inFiles;
    string mainName = "MCDC";
    string mainFileName = "mcdc";
  string solver;
    string[] namespaces;
    string[] compileDb;
    string out_;
    bool shortPluginHelp;
    bool help;

    private GetoptResult help_info;

    void parse(string[] args) {
        static import std.getopt;

        try {
            // dfmt off
      help_info = getopt(args, std.getopt.config.keepEndOfOptions,
			 "compile-db", "Retrieve compilation parameters from the file", &compileDb,
			 "ns", "namespaces to work on", &namespaces,
			 "in", "Input file to parse", &inFiles,
			 "solver", "absolute path to solverz3.py", &solver,
			 "main-fname", "Used as part of filename for generated files [default: mcdc]", &mainFileName,
			 "out", "directory for generated files [default: ./]", &out_,
			 "short-plugin-help", "short description of the plugin",  &shortPluginHelp,);
      // dfmt on
            help = help_info.helpWanted;
        } catch (std.getopt.GetOptException ex) {
            logger.error(ex.msg);
            help = true;
        }

    }

    void printHelp() {
        import std.stdio : writeln;

        defaultGetoptPrinter("Usage: dextool-cppmcdc [--solver=] [--ns=][options] [--in=] [--compile-db=]",
                help_info.options);
    }
}
