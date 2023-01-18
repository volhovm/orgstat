{-# LANGUAGE TemplateHaskell #-}

-- | Reexports orgmode-parse types & provides lenses for them. The
-- main AST related types & lenses. Maybe needs to be renamed.

module OrgStat.Org
  ( O.Document
  , documentText
  , documentHeadlines

  , O.Headline
  , depth
  , stateKeyword
  , priority
  , title
  , timestamp
  , stats
  , tags
  , section
  , subHeadlines

  )
  where

import qualified Data.OrgMode.Types as O

import Control.Lens.TH

makeLensesFor [("documentText", "documentText"),
               ("documentHeadlines", "documentHeadlines")]
    ''O.Document

makeLensesFor [("depth", "depth"),
               ("stateKeyword", "stateKeyword"),
               ("priority", "priority"),
               ("title", "title"),
               ("timestamp", "timestamp"),
               ("stats", "stats"),
               ("tags", "tags"),
               ("section", "section"),
               ("subHeadlines", "subHeadlines")]
    ''O.Headline

makeLensesFor [("sectionTimestamp", "sectionTimestamp"),
               ("sectionPlannings", "plannings"),
               ("sectionClocks", "sectionClocks"),
               ("sectionProperties", "propreties"),
               ("sectionLogbook", "logbook"),
               ("sectionDrawers", "drawers"),
               ("sectionParagraph", "paragraph")]
    ''O.Section
