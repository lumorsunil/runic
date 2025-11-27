import XCTest
import SwiftTreeSitter
import TreeSitterRunic

final class TreeSitterRunicTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_runic())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Runic grammar")
    }
}
