/**
   Copyright: Copyright (c) 2019, Oscar Holm. All rights reserved.
   License: MPL-2
   Author: Oscar Holm (oscar_holm94@hotmail.com)

   This Source Code Form is subject to the terms of the Mozilla Public License,
   v.2.0. If a copy of the MPL was not distributed with this file, You can obtain
   one at http://mozilla.org/MPL/2.0/.
*/
module dextool.plugin.cppmcdc.backend.interface_;

import dsrcgen.cpp : CppModule, CppHModule;

import dextool.type : AbsolutePath, FileName, DirName, MainName, DextoolVersion;

/** Parameters used during generation.
 *
 * Important aspact that they do NOT change, therefore it is pure.
 */
@safe pure interface Parameters {
    string[] getNamespaces();

    /// Dextool Tool version.
    FileName[] getIncludes();
    DextoolVersion getToolVersion();
    MainName getMainName();
  string getSolverPath();
}

/// Data produced by the generator like files.
@safe interface Products {
    void putFile(AbsolutePath fname, CppHModule hdr_data);
    void putFile(AbsolutePath fname, CppModule impl_data);
}

/** Transformations that are governed by user input or other factors the
 * backend is unaware of.
 */
@safe interface Transform {
    /// Returns: the transformed name to a filename suitable for a header.
    AbsolutePath createHeaderFile(string name);

    /// Returns: the transformed name to a filename suitable for an implementation.
    AbsolutePath createImplFile(string name);
}
