(lang dune 1.11)

(name as8)
(source (github jdrprod/as8))
(license MIT)
(authors "Arthur Correnson <arthur.correnson@gmail.com>")
(maintainers "Arthur Correnson <arthur.correnson@gmail.com>")

(package
 (name as8)
 (synopsis "Reliable assembler and tool-chain for CHIP8 assembly language")
 (description "
  This package provides all tools needed for CHIP8 programming including parser, assembler, debugger, VM etc.
 ")
 (depends
  (cmdliner (>= 1.0.3))
  (dune (>= 1.11))))