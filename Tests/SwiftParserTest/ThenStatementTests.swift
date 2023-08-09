//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(RawSyntax) @_spi(ExperimentalLanguageFeatures) import SwiftSyntax
@_spi(RawSyntax) @_spi(ExperimentalLanguageFeatures) import SwiftParser
import XCTest

final class ThenStatementTests: ParserTestCase {
  // Enable then statements by default.
  override var experimentalFeatures: Parser.ExperimentalFeatures {
    return .thenStatements
  }

  func testThenStmt1() {
    assertParse(
      """
      then 0
      """,
      substructure: Syntax(ThenStmtSyntax(expression: IntegerLiteralExprSyntax(0)))
    )
  }

  func testThenStmt2() {
    assertParse(
      """
      then x
      """,
      substructure: Syntax(ThenStmtSyntax(expression: DeclReferenceExprSyntax(baseName: "x")))
    )
  }

  func testThenStmt3() {
    assertParse(
      """
      then ()
      """,
      substructure: Syntax(ThenStmtSyntax(expression: TupleExprSyntax(elements: .init([]))))
    )
  }

  func testThenStmt4() {
    assertParse(
      """
      then (1)
      """,
      substructure: Syntax(
        ThenStmtSyntax(
          expression: TupleExprSyntax(
            elements: .init([
              .init(expression: IntegerLiteralExprSyntax(1))
            ])
          )
        )
      )
    )
  }

  func testThenStmt5() {
    assertParse(
      """
      then (1, 2)
      """,
      substructure: Syntax(
        ThenStmtSyntax(
          expression: TupleExprSyntax(
            elements: .init([
              .init(expression: IntegerLiteralExprSyntax(1), trailingComma: .commaToken()),
              .init(expression: IntegerLiteralExprSyntax(2)),
            ])
          )
        )
      )
    )
  }

  func testThenStmt6() {
    assertParse(
      """
      then ""
      """,
      substructure: Syntax(ThenStmtSyntax(expression: StringLiteralExprSyntax(content: "")))
    )
  }

  func testThenStmt7() {
    assertParse(
      """
      then []
      """,
      substructure: Syntax(ThenStmtSyntax(expression: ArrayExprSyntax(elements: .init(expressions: []))))
    )
  }

  func testThenStmt8() {
    assertParse(
      """
      then [0]
      """,
      substructure: Syntax(
        ThenStmtSyntax(
          expression: ArrayExprSyntax(
            elements: .init(expressions: [
              .init(IntegerLiteralExprSyntax(0))
            ])
          )
        )
      )
    )
  }

  func testThenStmt9() {
    assertParse(
      """
      then if .random() { 0 } else { 1 }
      """
    )
  }

  func testThenStmt10() {
    assertParse(
      """
      then -1
      """,
      substructure: Syntax(
        ThenStmtSyntax(
          expression: PrefixOperatorExprSyntax(
            operator: .prefixOperator("-"),
            expression: IntegerLiteralExprSyntax(1)
          )
        )
      )
    )
  }

  func testThenStmt11() {
    assertParse(
      """
      then ~1
      """,
      substructure: Syntax(
        ThenStmtSyntax(
          expression: PrefixOperatorExprSyntax(
            operator: .prefixOperator("~"),
            expression: IntegerLiteralExprSyntax(1)
          )
        )
      )
    )
  }

  func testThenStmt12() {
    assertParse(
      """
      then /.../
      """,
      substructure: Syntax(ThenStmtSyntax(expression: RegexLiteralExprSyntax(regex: .regexLiteralPattern("..."))))
    )
  }

  func testThenStmt13() {
    // This is a a statement.
    assertParse(
      """
      then .foo
      """,
      substructure: Syntax(ThenStmtSyntax(expression: MemberAccessExprSyntax(name: .identifier("foo"))))
    )
  }

  func testThenStmt14() {
    // This is a member access.
    assertParse(
      """
      then.foo
      """,
      substructure: Syntax(
        MemberAccessExprSyntax(
          base: DeclReferenceExprSyntax(baseName: .identifier("then")),
          name: .identifier("foo")
        )
      )
    )
  }

  func testThenStmt15() {
    // This could be a member access too, but it seems rare enough to continue
    // parsing as a statement.
    assertParse(
      """
      then . foo
      """,
      substructure: Syntax(ThenStmtSyntax(expression: MemberAccessExprSyntax(name: .identifier("foo"))))
    )
  }

  func testThenStmt16() {
    // This will be diagnosed in ASTGen.
    assertParse(
      """
      a: then 0
      """,
      substructure: Syntax(
        LabeledStmtSyntax(
          label: .identifier("a"),
          statement: ThenStmtSyntax(expression: IntegerLiteralExprSyntax(0))
        )
      )
    )
  }

  func testThenStmt17() {
    // This is a function call.
    assertParse(
      """
      then()
      """,
      substructure: Syntax(
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          leftParen: .leftParenToken(),
          arguments: .init([]),
          rightParen: .rightParenToken()
        )
      )
    )
  }

  func testThenStmt18() {
    // This is a function call.
    assertParse(
      """
      then(0)
      """,
      substructure: Syntax(
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          leftParen: .leftParenToken(),
          arguments: .init([.init(expression: IntegerLiteralExprSyntax(0))]),
          rightParen: .rightParenToken()
        )
      )
    )
  }

  func testThenStmt19() {
    // This is a function call.
    assertParse(
      """
      then(x: 0)
      """,
      substructure: Syntax(
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          leftParen: .leftParenToken(),
          arguments: .init([.init(label: "x", expression: IntegerLiteralExprSyntax(0))]),
          rightParen: .rightParenToken()
        )
      )
    )
  }

  func testThenStmt20() {
    // This is a function call.
    assertParse(
      """
      then{}
      """,
      substructure: Syntax(
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          arguments: .init([]),
          trailingClosure: ClosureExprSyntax(statements: .init([]))
        )
      )
    )
  }

  func testThenStmt21() {
    // This is a function call.
    assertParse(
      """
      then {}
      """,
      substructure: Syntax(
        FunctionCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          arguments: .init([]),
          trailingClosure: ClosureExprSyntax(statements: .init([]))
        )
      )
    )
  }

  func testThenStmt22() {
    assertParse(
      """
      then1️⃣
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected expression in 'then' statement",
          fixIts: ["insert expression"]
        )
      ],
      fixedSource: "then <#expression#>"
    )
  }

  func testThenStmt23() {
    assertParse(
      """
      then1️⃣;
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "expected expression in 'then' statement",
          fixIts: ["insert expression"]
        )
      ],
      fixedSource: "then <#expression#>;"
    )
  }

  func testThenStmt24() {
    assertParse(
      """
      (then)
      """,
      substructure: Syntax(
        TupleExprSyntax(
          elements: .init([
            .init(expression: DeclReferenceExprSyntax(baseName: .identifier("then")))
          ])
        )
      )
    )
  }

  func testThenStmt25() {
    assertParse(
      """
      then
      0
      """,
      substructure: Syntax(ThenStmtSyntax(expression: IntegerLiteralExprSyntax(0)))
    )
  }

  func testThenStmt26() {
    assertParse(
      """
      let x = then
      """,
      substructure: Syntax(DeclReferenceExprSyntax(baseName: .identifier("then")))
    )
  }

  func testThenStmt27() {
    assertParse(
      """
      self.then
      """,
      substructure: Syntax(
        MemberAccessExprSyntax(
          base: DeclReferenceExprSyntax(baseName: .keyword(.self)),
          name: .identifier("then")
        )
      )
    )
  }

  func testThenStmt28() {
    assertParse(
      """
      then + 2
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            BinaryOperatorExprSyntax(operator: .binaryOperator("+")),
            IntegerLiteralExprSyntax(2),
          ])
        )
      )
    )
  }

  func testThenStmt29() {
    assertParse(
      """
      then+2
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            BinaryOperatorExprSyntax(operator: .binaryOperator("+")),
            IntegerLiteralExprSyntax(2),
          ])
        )
      )
    )
  }

  func testThenStmt30() {
    assertParse(
      """
      then = 2
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            AssignmentExprSyntax(),
            IntegerLiteralExprSyntax(2),
          ])
        )
      )
    )
  }

  func testThenStmt31() {
    assertParse(
      """
      then=2
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            AssignmentExprSyntax(),
            IntegerLiteralExprSyntax(2),
          ])
        )
      )
    )
  }

  func testThenStmt32() {
    assertParse(
      """
      then is Int
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            UnresolvedIsExprSyntax(),
            TypeExprSyntax(type: IdentifierTypeSyntax(name: .identifier("Int"))),
          ])
        )
      )
    )
  }

  func testThenStmt33() {
    assertParse(
      """
      then as Int
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            UnresolvedAsExprSyntax(),
            TypeExprSyntax(type: IdentifierTypeSyntax(name: .identifier("Int"))),
          ])
        )
      )
    )
  }

  func testThenStmt34() {
    assertParse(
      """
      then as? Int
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            UnresolvedAsExprSyntax(questionOrExclamationMark: .postfixQuestionMarkToken()),
            TypeExprSyntax(type: IdentifierTypeSyntax(name: .identifier("Int"))),
          ])
        )
      )
    )
  }

  func testThenStmt35() {
    assertParse(
      """
      then as! Int
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            UnresolvedAsExprSyntax(questionOrExclamationMark: .exclamationMarkToken()),
            TypeExprSyntax(type: IdentifierTypeSyntax(name: .identifier("Int"))),
          ])
        )
      )
    )
  }

  func testThenStmt36() {
    assertParse(
      """
      then ? 0 : 1
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            UnresolvedTernaryExprSyntax(thenExpression: IntegerLiteralExprSyntax(0)),
            IntegerLiteralExprSyntax(1),
          ])
        )
      )
    )
  }

  func testThenStmt37() {
    assertParse(
      """
      1️⃣try then 0
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "'try' must be placed on the produced expression",
          fixIts: ["move 'try' after 'then'"]
        )
      ],
      fixedSource: "then try 0"
    )
  }

  func testThenStmt38() {
    assertParse(
      """
      then try 0
      """
    )
  }

  func testThenStmt39() {
    assertParse(
      """
      then!
      """,
      substructure: Syntax(
        ForceUnwrapExprSyntax(
          expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
        )
      )
    )
  }

  func testThenStmt40() {
    assertParse(
      """
      then?
      """,
      substructure: Syntax(
        OptionalChainingExprSyntax(
          expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
        )
      )
    )
  }

  func testThenStmt41() {
    assertParse(
      """
      then?.foo
      """,
      substructure: Syntax(
        MemberAccessExprSyntax(
          base: OptionalChainingExprSyntax(
            expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
          ),
          name: .identifier("foo")
        )
      )
    )
  }

  func testThenStmt42() {
    assertParse(
      """
      then!.foo
      """,
      substructure: Syntax(
        MemberAccessExprSyntax(
          base: ForceUnwrapExprSyntax(
            expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
          ),
          name: .identifier("foo")
        )
      )
    )
  }

  func testThenStmt43() {
    assertParse(
      """
      self.then(0)
      """,
      substructure: Syntax(
        MemberAccessExprSyntax(
          base: DeclReferenceExprSyntax(baseName: .keyword(.self)),
          name: .identifier("then")
        )
      )
    )
  }

  func testThenStmt44() {
    assertParse(
      """
      then /^ then/
      """,
      substructure: Syntax(
        SequenceExprSyntax(
          elements: .init([
            DeclReferenceExprSyntax(baseName: .identifier("then")),
            BinaryOperatorExprSyntax(operator: .binaryOperator("/^")),
            PostfixOperatorExprSyntax(
              expression: DeclReferenceExprSyntax(baseName: .identifier("then")),
              operator: .postfixOperator("/")
            ),
          ])
        )
      )
    )
  }

  func testThenStmt45() {
    assertParse(
      """
      return then
      """,
      substructure: Syntax(
        ReturnStmtSyntax(expression: DeclReferenceExprSyntax(baseName: .identifier("then")))
      )
    )
  }

  func testThenStmt46() {
    assertParse(
      """
      then[0]
      """,
      substructure: Syntax(
        SubscriptCallExprSyntax(
          calledExpression: DeclReferenceExprSyntax(baseName: .identifier("then")),
          arguments: .init([
            .init(expression: IntegerLiteralExprSyntax(0))
          ])
        )
      )
    )
  }

  func testThenStmt47() {
    assertParse(
      """
      then: for then in [] {
        break then
        continue then
      }
      """
    )
  }

  func testThenStmt48() {
    assertParse(
      """
      throw then
      """,
      substructure: Syntax(
        ThrowStmtSyntax(expression: DeclReferenceExprSyntax(baseName: .identifier("then")))
      )
    )
  }

  func testThenStmt49() {
    assertParse(
      """
      try then()
      """
    )
  }

  func testThenStmt50() {
    assertParse(
      """
      try then{}
      """
    )
  }

  func testThenStmt51() {
    assertParse(
      """
      try then {}
      """
    )
  }

  func testThenStmt52() {
    assertParse(
      """
      try then + 1
      """
    )
  }

  func testThenStmt53() {
    assertParse(
      """
      then
        .foo
      """,
      substructure: Syntax(ThenStmtSyntax(expression: MemberAccessExprSyntax(name: .identifier("foo"))))
    )
  }

  func testThenStmt54() {
    assertParse(
      """
      return try then
      """,
      substructure: Syntax(
        ReturnStmtSyntax(
          expression: TryExprSyntax(
            expression: DeclReferenceExprSyntax(baseName: .identifier("then"))
          )
        )
      )
    )
  }

  func testThenStmt55() {
    assertParse(
      """
      let x = [
        0,
        then
      ]
      """
    )
  }

  func testThenStmtDisabled1() {
    // Make sure it's disabled by default.
    assertParse(
      """
      then1️⃣ 0
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "consecutive statements on a line must be separated by newline or ';'",
          fixIts: [
            "insert newline",
            "insert ';'",
          ]
        )
      ],
      fixedSource: """
        then
        0
        """,
      experimentalFeatures: []
    )
  }

  func testThenStmtDisabled2() {
    // Make sure it's disabled by default. This is specifically testing
    // StmtSyntax.parse, since it will try to parse without checking
    // `atStartOfThenStatement`.
    var parser = Parser("then 0")
    let invalid = StmtSyntax.parse(from: &parser)
    XCTAssert(invalid.hasError)
  }
}
