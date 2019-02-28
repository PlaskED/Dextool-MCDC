/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/
module dextool.plugin.cppmcdc.frontend.frontend;

import logger = std.experimental.logger;

import dextool.compilation_db;

import dextool.type : AbsolutePath, CustomHeader, WriteStrategy, DextoolVersion,
    ExitStatusType, FileName, InFiles, MainName, MainFileName, MainNs;

import dextool.plugin.cppmcdc.backend : Parameters, Products, Transform;
import dextool.plugin.cppmcdc.frontend.raw_args : RawConfiguration;

struct FileData {
    AbsolutePath filename;
    string data;
    WriteStrategy strategy;
}

class CppMcdcVariant : Parameters, Products {
    import dsrcgen.cpp;
    import dextool.compilation_db : CompileCommandFilter;

    private {

        MainName mainName;
        MainNs mainNs;
        FileName[] includes;
        FileData[] fileData;
        string[] namespaces;
	string solver;

        CompileCommandFilter compiler_flag_filter;
    }

    static auto makeVariant(ref RawConfiguration args) {
        auto variant = new CppMcdcVariant(args.namespaces)
	   // dfmt off
	  .argMainName(args.mainName)
	  .argSolver(args.solver)
	  .argCompileFilter();
 	   // dfmt on
        return variant;
    }

    this(string[] namespaces) {
        this.namespaces = namespaces;
    }

    auto argMainName(string s) {
        this.mainName = MainName(s);
        this.mainNs = MainNs(s);
        return this;
    }

    auto argSolver(string s) {
      this.solver = s;
      return this;
    }

    auto argCompileFilter() {
        compiler_flag_filter = CompileCommandFilter(defaultCompilerFlagFilter, 1);
        return this;
    }

    ref CompileCommandFilter getCompileCommandFilter() {
        return compiler_flag_filter;
    }

    // -- Parameters -- //

    string[] getNamespaces() {
        return namespaces;
    }

    FileName[] getIncludes() {
        import std.algorithm : map;
        import std.array : array;

        return includes.map!(a => FileName(a)).array();
    }

    MainName getMainName() {
        return mainName;
    }

    DextoolVersion getToolVersion() {
        import dextool.utility : dextoolVersion;

        return dextoolVersion;
    }

  string getSolverPath() {
    return solver;
  }

    // -- Products --

    void putFile(AbsolutePath fname, CppHModule hdr_data) {
        fileData ~= FileData(fname, hdr_data.render());
    }

    void putFile(AbsolutePath fname, CppModule impl_data) {
        fileData ~= FileData(fname, impl_data.render());
    }
}

class FrontendTransform : Transform {
    import std.path : buildPath;
    import dextool.type : AbsolutePath, DirName, FileName;

    static const hdrExt = ".hpp";
    static const implExt = ".cpp";

    DirName output_dir;
    MainFileName main_fname;

    this(MainFileName main_fname, DirName output_dir) {
        this.main_fname = main_fname;
        this.output_dir = output_dir;
    }

    AbsolutePath createHeaderFile(string name) {
        return AbsolutePath(FileName(buildPath(output_dir, main_fname ~ name ~ hdrExt)));
    }

    AbsolutePath createImplFile(string name) {
        return AbsolutePath(FileName(buildPath(output_dir, main_fname ~ name ~ implExt)));
    }
}

ExitStatusType genCpp(CppMcdcVariant variant, FrontendTransform transform,
        CompileCommandDB compile_db, InFiles in_files) {
    import dextool.clang : findFlags;
    import dextool.io : writeFileData;
    import dextool.type : AbsolutePath;
    import dextool.compilation_db : ParseData = SearchResult;
    import dextool.plugin.cppmcdc.backend : Backend;
    import dextool.utility : prependDefaultFlags, PreferLang;
    
    string[] in_cflags;
    const auto total_files = in_files.length + compile_db.length;
    const auto user_cflags = prependDefaultFlags(in_cflags, PreferLang.cpp);
    auto generator = Backend(variant, variant, transform);

    if (in_files.length > 0) {
        foreach (idx, in_file; in_files) {
	  
	  logger.infof("Files %d/%d ", idx + 1, total_files);
	  ParseData pdata;
	  pdata.flags.prependCflags(user_cflags.dup);
	  pdata.absoluteFile = AbsolutePath(FileName(in_file));
	  if (generator.analyzeFile(pdata.absoluteFile, pdata.cflags) == ExitStatusType.Errors) {
	    return ExitStatusType.Errors;
	  }
	}
    }

    // Analyze files in compilation database
    if (compile_db.length > 0) {
        foreach (idx, cmd; compile_db) {
            logger.infof("File %d/%d ", idx + 1, total_files);

            auto db_search = compile_db.appendOrError(user_cflags, cmd.absoluteFile,
                    CompileCommandFilter(defaultCompilerFlagFilter, 1));

            if (db_search.isNull) {
                return ExitStatusType.Errors;
            }

            if (generator.analyzeFile(db_search.absoluteFile,
                    db_search.cflags) == ExitStatusType.Errors) {
                return ExitStatusType.Errors;
            }
        }
    }

    // Process and generate artifacts.
    generator.process();

    return writeFileData(variant.fileData);
}
