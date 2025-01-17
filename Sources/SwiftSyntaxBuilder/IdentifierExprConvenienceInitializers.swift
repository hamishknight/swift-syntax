//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension IdentifierExpr {
  public init(_ identifier: String) {
    self.init(identifier: .identifier(identifier))
  }
}

extension IdentifierExpr: ExpressibleByStringLiteral {
  public init(stringLiteral value: String) {
    self.init(value)
  }
}
