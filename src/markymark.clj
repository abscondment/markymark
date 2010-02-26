(ns markymark
  (:use
   [clojure.contrib pprint]
   [com.lithinos.clj-peg core string-wrapper]))

(make-parser
 {:main markdown
  :doc "blah"
  :rules (Doc            <- [(* Block)]
          Block          <- [(* BlankLine) (| BlockQuote
                                              Verbatim
                                              HorizontalRule
                                              Heading
                                              
                                              Para
                                              Plain)]

          BlockQuote     <- (+ [GT (? SpOne) Line
                                (* [(! GT) (! BlankLine) Line])
                                (* BlankLine)])
          Verbatim       <- (+ [(* BlankLine)
                                (+ NonblankIndentedLine)])
          HorizontalRule <- [NonindentSpace
                             (| [AST Sp AST Sp AST (* [Sp AST])]
                                [Dash Sp Dash Sp Dash (* [Sp Dash])]
                                [UND Sp UND Sp UND (* [Sp UND])])
                             Sp
                             Newline
                             (+ BlankLine)]
          Heading        <- (| AtxHeading SetextHeading)
          
          Para           <- [NonindentSpace Inlines (+ BlankLine)]
          Plain          <- Inline


          Inline         <- #".+"
          Inlines        <- (+ Inline)


          

          SetextHeading  <- (| SetextHeadingE
                               SetextHeadingD)
          SetextHeadingE <- [(+ [(! EndLine) Inline])
                             Newline
                             EqThree
                             (* EqOne)
                             Newline]
          SetextHeadingD <- [(+ [(! EndLine) Inline])
                             Newline
                             DashThree
                             (* Dash)
                             Newline]
          
          NumOne         <- "#"
          NumTwo         <- "##"
          NumThree       <- "###"
          NumFour        <- "####"
          NumFive        <- "#####"
          NumSix         <- "######"
          AtxStart       <- (| NumSix NumFive NumFour NumThree NumTwo NumOne)
          AtxInline      <- [(! Newline)
                             (! [Sp (* NumOne) Newline])
                             Inline]
          AtxHeading     <- [AtxStart
                             Sp
                             (+ AtxInline)
                             (? [Sp (* NumOne) Sp])
                             Newline]
           
          Space          <- " "
          Tab            <- "\t"
          Spacechar      <- (| Space Tab)
          Sp             <- (* Spacechar)
          SpOne          <- " "
          SpTwo          <- "  "
          SpThree        <- "   "
          SpFour         <- "    "
          NonindentSpace <- (| SpThree SpTwo SpOne (! Spacechar))
          Indent         <- (| Tab SpFour)
          LT             <- "<"
          GT             <- ">"
          AST            <- "*"
          Dash           <- "-"
          UND            <- "_"
          EqOne          <- "="
          EqThree        <- "==="
          DashThree      <- "---"
          
          NormalEndline  <- [Sp
                             Newline
                             (! BlankLine)
                             (! GT)
                             (! AtxStart)
                             (! [Line
                                 (| [EqThree (* EqOne)]
                                    [DashThree (* Dash)])
                                 Newline])]
          TerminalEndline <- [Sp Newline $]
          LineBreak      <- [SpTwo NormalEndline]
          EndLine        <- (| LineBreak
                               TerminalEndline
                               NormalEndline)
          
          NL             <- "\n"
          CR             <- "\r"
          Newline        <- (| NL [CR (? NL)])
          Line           <- (| [(* [(! CR) (! NL) Any])
                                Newline]
                               [(+ Any) $])
          BlankLine      <- [Sp Newline]
          IndentedLine   <- [Indent Line]
          OptionallyIndentedLine <- [(? Indent) Line]
          NonblankIndentedLine   <- [(! BlankLine)
                                     IndentedLine]
          
          Any            <- #".+"
          )})



(comment
  (time
 (dotimes [_ 100]
   (markdown
    (wrap-string "Get that man a *drink*.

OK?")))))


(time
 (pprint
  (markdown
   (wrap-string "
# DAH

## Uan

MAN!
=====

OK?
"))))
