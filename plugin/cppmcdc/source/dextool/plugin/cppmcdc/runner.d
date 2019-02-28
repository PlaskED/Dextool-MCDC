/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/

module dextool.plugin.runner;

auto runPlugin(string[] args) {
    import logger = std.experimental.logger;
    import std.stdio : writeln;
    import dextool.compilation_db;
    import dextool.type;
    import dextool.utility;
    import dextool.plugin.cppmcdc.frontend : genCpp, CppMcdcVariant,
        RawConfiguration, FrontendTransform;

    RawConfiguration pargs;
    pargs.parse(args);

    if (pargs.shortPluginHelp) {
        writeln("cppmcdc");
        writeln("generates MC/DC test cases from C++ code");
        return ExitStatusType.Ok;
    } else if (pargs.help) {
        pargs.printHelp;
        return ExitStatusType.Ok;
    } else if (pargs.namespaces.length == 0) {
        writeln("Specify one ore more namespaces [--ns]");
        return ExitStatusType.Errors;
    } else if (pargs.inFiles.length == 0 && pargs.compileDb.length == 0) {
        writeln("Use at least one of: [--in=] [--compile-db=]");
        return ExitStatusType.Errors;
    } else if (pargs.solver.length == 0) {
      writeln("Specifiy path to solver [--solver=]");
      return ExitStatusType.Errors;
    }

    CompileCommandDB compile_db;
    if (pargs.compileDb.length != 0) {
        try {
            compile_db = pargs.compileDb.fromArgCompileDb;
        } catch (Exception e) {
            logger.error(e.msg);
            logger.error("Unable to open compile commands database(s)");
            return ExitStatusType.Errors;
        }
    }

    auto transform = new FrontendTransform(MainFileName(pargs.mainFileName), DirName(pargs.out_));

    auto variant = CppMcdcVariant.makeVariant(pargs);

    return genCpp(variant, transform, compile_db, InFiles(pargs.inFiles));
}
