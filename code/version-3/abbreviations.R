# This file contains logic responsible for converting abbreviations in the
# Excel match spreadsheet into their non-abbreviated form.
#
# An AbbreviationConverter_<NAME> class should be created for each column within
# the Excel match spreadsheet (ie "poss.action", "play.type", etc) that contains
# abbreviations that need converting. The AbbreviationConverter classes should
# define a list of abbreviations and their associated replacements.
#
# To "AbbreviationProcessor" class is the interface used to perform abbreviations
# for the rows and columns from an Excel match spreadsheet.
#
library(R6)

# AbbreviationConverter for the "poss.action" column.
AbbreviationConverter_PossAction <-
  R6Class("Abbreviation_PossAction", inherit = AbbreviationConverter_Base, 
          public = list(column_name = "poss.action", 
                        initialize = function() {
                          self$add_abbreviation("^sgk",                  "shots.stopped.by.gk")
                          self$add_abbreviation("^sdef",                 "shots.stopped.by.def")
                          self$add_abbreviation("^sb",                   "shots.blocked")
                          self$add_abbreviation("^sc",                   "shots.scored")
                          self$add_abbreviation("^sm",                   "shots.missed")
                          self$add_abbreviation("^pf",                   "passes.f")
                          self$add_abbreviation("^ps",                   "passes.s")
                          self$add_abbreviation("^pb",                   "passes.b")
                          self$add_abbreviation("^m",                    "movement")
                          self$add_abbreviation("^tkw",                  "take.on.won")
                          self$add_abbreviation("^tkl",                  "take.on.lost")
                          self$add_abbreviation("^d",                    "dispossessed")
                          self$add_abbreviation("^lt",                   "lost.touch")
                          self$add_abbreviation("^bt",                   "ball.touch")
                          self$add_abbreviation("^gw",                   "ground.50.50.won")
                          self$add_abbreviation("^gl",                   "ground.50.50.lost")
                          self$add_abbreviation("^aw",                   "aerial.won")
                          self$add_abbreviation("^al",                   "aerial.lost")
                          self$add_abbreviation("^r",                    "recoveries")
                          self$add_abbreviation("^bs",                   "ball.shield")
                          self$add_abbreviation("^cl$|^cl ",             "clearances")
                          self$add_abbreviation("^playcutoff",           "playcutoffbybroadcast")
                          self$add_abbreviation("^fw",                   "fouls.won")
                          self$add_abbreviation("^fc",                   "fouls.conceded")
                          }
                        )
          )
# AbbreviationConverter for the "play.type" column.
AbbreviationConverter_PlayType <- 
  R6Class("Abbreviation_PlayType", inherit = AbbreviationConverter_Base,
          public = list(column_name = "play.type", 
                        initialize = function() {
                          self$add_abbreviation("^th|^th$",             "through")
                          self$add_abbreviation("^cc|^cr|^dc",             "crosses")
                          self$add_abbreviation("^s",                      "switch")
                          self$add_abbreviation("^lau",                    "launch")
                          self$add_abbreviation("^ti",                     "throw.in")
                          self$add_abbreviation("^fk",                     "free.kick")
                          self$add_abbreviation("^h",                      "headed")
                          self$add_abbreviation("^ck",                     "corner.kick")
                          self$add_abbreviation("^gk$|gk ",                "goal.kick")
                          self$add_abbreviation("^gkt",                    "gk.throws")
                          self$add_abbreviation("^gkdk",                   "gk.drop.kick")
                          self$add_abbreviation("^pk",                     "penalty.kick")
                          self$add_abbreviation("^pip",                    "pass.into.pressure")
                          self$add_abbreviation("^keep.poss|^kept.poss",   "out.of.bounds.keep.poss")
                          self$add_abbreviation("^lost.poss|^lose.poss",   "out.of.bounds.lost.poss")
                          }
                        )
          )
# AbbreviationConverter for the "def.action" column.
AbbreviationConverter_DefAction <-
  R6Class("Abbreviation_DefAction", inherit = AbbreviationConverter_Base,
          public = list(column_name = "def.action",
                        initialize = function() {
                          self$add_abbreviation("^dbs|^bs",            "ball.shield")
                          self$add_abbreviation("^dis|^ds|^dlt",       "dispossessed")
                          self$add_abbreviation("^tb|^tba|^tbw",       "tackles.ball")
                          self$add_abbreviation("^dtm|^dor|^dt|^dr",   "dribbled")
                          self$add_abbreviation("^p",                  "pressured")
                          self$add_abbreviation("^ch",                 "challenged")
                          self$add_abbreviation("^bl",                 "blocks")
                          self$add_abbreviation("^int",                "interceptions")
                          self$add_abbreviation("^bd",                 "ball.shield")
                          self$add_abbreviation("^cl",                 "clearances")
                          self$add_abbreviation("^cl.h.p$|^cl.p.h$",   "clearances.headed.pressed")
                          self$add_abbreviation("^cl.h$",              "clearances.headed")
                          self$add_abbreviation("^cl.p$",              "clearances.pressed")
                          self$add_abbreviation("^aw",                 "aerial.won")
                          self$add_abbreviation("^al",                 "aerial.lost")
                          self$add_abbreviation("^fw",                 "fouls.won")
                          self$add_abbreviation("^fc",                 "fouls.conceded")
                          self$add_abbreviation("^bt",                 "ball.touch")
                          self$add_abbreviation("^gw",                 "ground.50.50.won")
                          self$add_abbreviation("^gl",                 "ground.50.50.lost")
                          }
                        )
          )
# AbbreviationConverter for the "poss.player.disciplinary" column.
AbbreviationConverter_PossPlayerDisciplinary <- 
  R6Class("PossPlayerDisciplinary", inherit = AbbreviationConverter_Base,
          public = list(column_name = "poss.player.disciplinary",
                        initialize = function() {
                          self$add_abbreviation("^fw",   "fouls.won")
                          self$add_abbreviation("^fc",   "fouls.conceded")
                          }
                        )
          )
# AbbreviationConverter for the "def.player.disciplinary" column.
AbbreviationConverter_DefPlayerDisciplinary <-
  R6Class("DefPlayerDisciplinary", inherit = AbbreviationConverter_Base, 
          public = list(column_name = "def.player.disciplinary",
                        initialize = function() {
                          self$add_abbreviation("^fw",   "fouls.won")
                          self$add_abbreviation("^fc",   "fouls.conceded")
                          }
                        )
          )
# AbbreviationConverter for the "poss.notes" column.
AbbreviationConverter_PossNotes <-
  R6Class("PossNotes", inherit = AbbreviationConverter_Base, 
          public = list(column_name = "poss.notes",
                        initialize = function() {
                          self$add_abbreviation("^keep.poss|^kept.poss",   "out.of.bounds.keep.poss")
                          self$add_abbreviation("^lost.poss|^lose.poss",   "out.of.bounds.lost.poss")
                          }
                        )
          )
# AbbreviationConverter_Base:
# Contains shared logic that all derived AbbreviationConverter
# classes use for abbreviation replacement.
AbbreviationConverter_Base <-
  R6Class("AbbreviationConverter_Base",
          public = list(abbreviations = list(),
                        # @brief  Searches list of abbreviations determining if any abbreviation
                        #         within matches @value and is eligible for replacement.
                        #
                        # @param value  The value to check for replacement.
                        convert = function(value) {
                          for (abbreviation in self$abbreviations) {
                            if (grepl(abbreviation[1], value)) {
                              return(abbreviation[2])
                            }
                          }
                          return(value)
                        },
                        # @brief  Adds an abbreviation and its replacement to the abbreviation list.
                        #
                        # @param abbrev   The abbreviated value (regex) that should be replaced.
                        # @param replace  The value to use as the replacement.
                        add_abbreviation = function(abbrev, replace) {
                          self$abbreviations[[length(self$abbreviations)+1]] = c(abbrev, replace)
                        }
                        )
          )
# AbbreviationProcessor
#
# The interface used for replacing abbreviations in an
# Excel match spreadsheet.
AbbreviationProcessor <-
  R6Class("AbbreviationProcessor", 
          public = list(abbreviation_converters = list(), 
                        initialize = function(){
                          self$abbreviation_converters[[1]] = AbbreviationConverter_PossAction$new()
                          self$abbreviation_converters[[2]] = AbbreviationConverter_PlayType$new()
                          self$abbreviation_converters[[3]] = AbbreviationConverter_DefAction$new()
                          self$abbreviation_converters[[4]] = AbbreviationConverter_PossPlayerDisciplinary$new()
                          self$abbreviation_converters[[5]] = AbbreviationConverter_DefPlayerDisciplinary$new()
                          self$abbreviation_converters[[6]] = AbbreviationConverter_PossNotes$new()
                          },
                        # @brief  Replaces all abbreviations in a single row from
                        #         an Excel match spreadsheet.
                        #
                        # @param row  The row to perform abbreviation replacements on.
                        process_row = function(row) {
                          for(converter in self$abbreviation_converters) {
                            row[converter$column_name] = converter$convert(row[converter$column_name])
                          }
                          return(row)
                        }
                        )
          )