a =
  [ DocEnvironment
      ( Environment
          { name = "Tex",
            begin = Nothing,
            end = Nothing,
            args = [],
            innerMath = False,
            innerVerb = Verb,
            insidePref = True
          }
      )
      []
      [DocVerb False ["% Include packages in `@Tex` environment to", "% prevent translator working with them as paragraph ", "\\documentclass[12pt,a4paper,oneside]{article}", "\\usepackage[utf8]{inputenc}"]],
    DocEmptyLine,
    DocEnvironment
      ( Environment
          { name = "Document",
            begin = Just "\\begin{document}",
            end = Just "\\end{document}",
            args = [],
            innerMath = False,
            innerVerb = NoVerb,
            insidePref = True
          }
      )
      []
      [ DocParagraph
          [ [ ParText
                [ ("Hello,", (SourcePos {sourceName = "./drafts/test.ttex", sourceLine = Pos 21, sourceColumn = Pos 3}, SourcePos {sourceName = "./drafts/test.ttex", sourceLine = Pos 21, sourceColumn = Pos 9})),
                  ("\\LaTeX!", (SourcePos {sourceName = "./drafts/test.ttex", sourceLine = Pos 21, sourceColumn = Pos 10}, SourcePos {sourceName = "./drafts/test.ttex", sourceLine = Pos 21, sourceColumn = Pos 17}))
                ]
            ]
          ],
        DocEnvironment
          ( Environment
              { name = "Verbatim",
                begin = Just "\\begin{verbatim}",
                end = Just "\\end{verbatim}",
                args = [],
                innerMath = False,
                innerVerb = VerbIndent,
                insidePref = True
              }
          )
          []
          [DocVerb True ["\\begin{enumerate}", "  \\item one", "  \\item two", "\\end{enumerate}"]],
        DocEmptyLine,
        DocEnvironment
          ( Environment
              { name = "Verbatim'",
                begin = Just "\\begin{verbatim}",
                end = Just "\\end{verbatim}",
                args = [],
                innerMath = False,
                innerVerb = Verb,
                insidePref = True
              }
          )
          []
          [ DocVerb
              False
              ["\\begin{enumerate}", "  \\item one", "  \\item two", "\\end{enumerate}"]
          ],
        DocEmptyLine,
        DocParagraph
          [ [ ParFormula
                [ ( "a",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 4
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 5
                        }
                    )
                  ),
                  ( "+",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 6
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 7
                        }
                    )
                  ),
                  ( "b",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 8
                        },
                      SourcePos {sourceName = "./drafts/test.ttex", sourceLine = Pos 34, sourceColumn = Pos 9}
                    )
                  )
                ],
              ParText
                [ ( "",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 10
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 10
                        }
                    )
                  )
                ],
              ParFormula
                [ ( "c",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 12
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 13
                        }
                    )
                  ),
                  ( "+",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 14
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 15
                        }
                    )
                  ),
                  ( "d",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 16
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 34,
                          sourceColumn = Pos 17
                        }
                    )
                  )
                ]
            ],
            [ ParFormula
                [ ( "e",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 35,
                          sourceColumn = Pos 4
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 35,
                          sourceColumn = Pos 5
                        }
                    )
                  ),
                  ( "+",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 35,
                          sourceColumn = Pos 6
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 35,
                          sourceColumn = Pos 7
                        }
                    )
                  ),
                  ( "f",
                    ( SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 35,
                          sourceColumn = Pos 8
                        },
                      SourcePos
                        { sourceName = "./drafts/test.ttex",
                          sourceLine = Pos 35,
                          sourceColumn = Pos 9
                        }
                    )
                  )
                ]
            ]
          ]
      ]
  ]
