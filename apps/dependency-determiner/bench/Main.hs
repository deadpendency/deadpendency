module Main
  ( main,
  )
where

import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Haskell.HaskellCabal
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Text.RawString.QQ
import Weigh

main :: IO ()
main = do
  let result = determineDependencies (GitPath "path") $ HaskellCabalInput theCabalFile
  putTextLn $ show result
  pure ()

theCabalFile :: Text
theCabalFile =
  [r|cabal-version:      3.0
Category:           Biology
build-type:         Simple
name:               phylogenetic-component-graph
version:            0.3.0

author:             Ward Wheeler
maintainer:         wheeler@amnh.org
license:            BSD-3-Clause
license-file:       LICENSE

synopsis:           Program and library for general phylogenetic graph search

description:        Phylogenetic Componet Graph provides a library for
                    interacting with phylogenetic graphs. It defines a
                    decalartive scripting language for defining phylogenetic
                    searches. Said scripts can be evaluated by the provided pcg
                    program to perform a phylogenetic search on an input dataset.

tested-with:
    GHC == 8.8.4
    GHC == 8.10.3

extra-source-files:
    AUTHORS.md
    CHANGELOG.md
    FUNDING.md
    README.md
    -- Specify the header files as required source files here.
    -- Do not specify them in the c-sources or cxx-sources stanzas.
    -- This is required for sdist and install commands to work correctly.
    lib/core/ffi/external-direct-optimization/alignCharacters.h
    lib/core/ffi/external-direct-optimization/alignmentMatrices.h
    lib/core/ffi/external-direct-optimization/c_alignment_interface.h
    lib/core/ffi/external-direct-optimization/c_code_alloc_setup.h
    lib/core/ffi/external-direct-optimization/costMatrix.h
    lib/core/ffi/external-direct-optimization/debug_constants.h
    lib/core/ffi/external-direct-optimization/dyn_character.h
    lib/core/ffi/external-direct-optimization/ukkCheckPoint.h
    lib/core/ffi/external-direct-optimization/ukkCommon.h
    lib/tcm-memo/ffi/memoized-tcm/costMatrix_2d.hpp
    lib/tcm-memo/ffi/memoized-tcm/costMatrix_3d.hpp
    lib/tcm-memo/ffi/memoized-tcm/costMatrixWrapper_2d.h
    lib/tcm-memo/ffi/memoized-tcm/costMatrixWrapper_3d.h
    lib/tcm-memo/ffi/memoized-tcm/costMatrixWrapper.h
    lib/tcm-memo/ffi/memoized-tcm/dynamicCharacterOperations.h


-- Group of buildinfo specifications to correctly build and link to the C & C++:
-- FFI code.
common ffi-buildinfo

  cc-options:
    --std=c11

  hs-source-dirs:
    lib/core/ffi

  c-sources:
    lib/core/ffi/external-direct-optimization/alignCharacters.c
    lib/core/ffi/external-direct-optimization/alignmentMatrices.c
    lib/core/ffi/external-direct-optimization/c_alignment_interface.c
    lib/core/ffi/external-direct-optimization/c_code_alloc_setup.c
    lib/core/ffi/external-direct-optimization/costMatrix.c
    lib/core/ffi/external-direct-optimization/dyn_character.c
    lib/core/ffi/external-direct-optimization/ukkCheckPoint.c
    lib/core/ffi/external-direct-optimization/ukkCommon.c

  -- Here we list all directories that contain C & C++ header files that the FFI
  -- tools will need to locate when preprocessing the C files. Without listing
  -- the directories containing the C header files here, the FFI preprocessor
  -- (hsc2hs, c2hs, etc.) will fail to locate the requisite files. Note also,
  -- that the parent directory of the nessicary C & C++ header files must be
  -- specified. The preprocessor will not recursively look in subdirectories for
  -- header files!
  include-dirs:
    lib/core/ffi/external-direct-optimization


-- A litany of GHC warnings designed to alert us during the build of any common
-- pitfalls, future compatibility issues, or coding conventions.
common ghc-flags

  ghc-options:
--    -debug
--    -rtsopts
--    -g
    -- Optimization flags
    -O2
    -fexcess-precision
    -fexpose-all-unfoldings
    -flate-specialise
    -foptimal-applicative-do
    -fspecialize-aggressively
    -fstatic-argument-transformation
    -- Usability flags
    -fdiagnostics-color=always
    -fhide-source-paths
    -j
    -- Sanity check warnings
    -Wall
    -Wcompat
    -Wdodgy-foreign-imports
    -Wduplicate-exports
    -Wempty-enumerations
    -Widentities
    -Wincomplete-patterns
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmissed-specialisations
    -Wmissing-deriving-strategies
    -Wmissing-fields
    -Wmissing-home-modules
    -Wmissing-monadfail-instances
    -Wmissing-signatures
    -Wnoncanonical-monad-instances
    -Wnoncanonical-monoid-instances
    -Woverflowed-literals
    -Woverlapping-patterns
    -Wredundant-constraints
    -Wsemigroup
    -Wtabs
    -Wunrecognised-warning-flags
    -Wunused-binds
    -Wunused-do-bind
    -Wunused-foralls
    -Wunused-imports
    -Wunused-matches
    -Wwrong-do-bind

  if impl(ghc >= 8.10)
    ghc-options:
      -Wderiving-defaults
      -Wunused-packages


-- Global deviations from Haskell98
common language-specs

  -- Always use MonadFail(fail), not Monad(fail)
  other-extensions:
    MonadFailDesugaring
    DerivingStrategies


library alphabet

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/alphabet/src

  build-depends:
    serialize,
    utility,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 1.0,
    binary                   >= 0.8       && < 1.0,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    mtl                      >= 2.2.2     && < 3.0,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    transformers             >= 0.5.6     && < 1.0,

  -- Apparently this is needed to compile with profiling, which is really stupid.
  -- It should only be in the modules which use TemplateHaskell, not all modules.
  -- I consider this a temporary hack to get things to compile with profiling.
  other-extensions: TemplateHaskell

  exposed-modules:
    Data.Alphabet
    Data.Alphabet.IUPAC
    Data.Alphabet.Special

  other-modules:
    Data.Alphabet.Internal


-- Library for performing parsimony analysis.

-- Provides various metrics for scoring static characters and
-- performing string alignment on dynamic characters.

library analysis

  import:
    ffi-buildinfo,
    ghc-flags,
    language-specs

  visibility:
    private

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/core/analysis/src

  build-depends:
    data-structures,
    exportable,
    tcm,
    tcm-memo,
    utility,
    base                     >= 4.11      && < 5.0,
    clustering               >= 0.4       && < 0.5,
    containers               >= 0.6.2     && < 1.0,
    data-default             >= 0.5.2     && < 0.8,
    dlist                    >= 0.8       && < 1.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    matrices                 >= 0.5       && < 1.0,
    monad-loops              >= 0.4       && < 1.0,
    mono-traversable         >= 1.0       && < 2.0,
    mtl                      >= 2.2.2     && < 3.0,
    parallel                 >= 3.2       && < 4.0,
    unordered-containers     >= 0.2.10    && < 1.0,
    vector                   >= 0.12.0.3  && < 0.13,
    vector-builder           >= 0.3.7     && < 0.4,
    vector-instances         >= 3.4       && < 3.5,

  exposed-modules:
    Analysis.Clustering
    Analysis.Clustering.Hierarchical
    Analysis.Distance
    Analysis.Parsimony.Additive
    Analysis.Parsimony.Dynamic.DirectOptimization
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise
    Analysis.Parsimony.Fitch
    Analysis.Parsimony.Sankoff
    Analysis.Scoring
    Analysis.TotalEdgeCost

  other-modules:
    Analysis.Parsimony.Additive.Internal
    Analysis.Parsimony.Dynamic.DirectOptimization.Internal
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Internal
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Internal
    Analysis.Parsimony.Fitch.Internal
    Analysis.Parsimony.Sankoff.Internal
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.FFI
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.NeedlemanWunsch
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Ukkonen.Ribbon
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedFullMatrix
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedSwapping
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedUkkonenFullSpace
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.UnboxedUkkonenSwapping


library character-name

  import:
    ghc-flags,
    language-specs

  visibility:
    private

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/character-name/src

  build-depends:
    file-source,
    base                     >= 4.11      && < 5.0,
    binary                   >= 0.8       && < 1.0,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    mtl                      >= 2.2.2     && < 3.0,
    mono-traversable         >= 1.0       && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    text-show-instances      >= 3.8       && < 4.0,

  exposed-modules:
    Data.CharacterName


-- Data-types for defining and interacting with phylogenetic components.

-- Exposes core data-types for constructing, mutating, and scoring phylogenetic
-- components. Phylogenetic forests, the primitive component type, are defined
-- here along with the interface for interacting with phylogenetic forests.
-- Character sequences and matricies, character and graph metadata, and scoring
-- methods are defined as well.

library core

  import:
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  build-depends:
    analysis,
    data-structures,
    tcm

  reexported-modules:
    Analysis.Clustering,
    Analysis.Distance,
    Analysis.Parsimony.Additive,
    Analysis.Parsimony.Dynamic.DirectOptimization,
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise,
    Analysis.Parsimony.Fitch,
    Analysis.Parsimony.Sankoff,
    Analysis.Scoring,
    Analysis.TotalEdgeCost,
    Bio.Character,
    Bio.Character.Decoration.Additive,
    Bio.Character.Decoration.Continuous,
    Bio.Character.Decoration.Discrete,
    Bio.Character.Decoration.Discrete.Class,
    Bio.Character.Decoration.Dynamic,
    Bio.Character.Decoration.Fitch,
    Bio.Character.Decoration.Metric,
    Bio.Character.Decoration.NonMetric,
    Bio.Character.Encodable,
    Bio.Character.Encodable.Continuous,
    Bio.Metadata,
    Bio.Metadata.Continuous,
    Bio.Metadata.Discrete,
    Bio.Metadata.DiscreteWithTCM,
    Bio.Metadata.Dynamic,
    Bio.Metadata.Metric,
    Bio.Sequence,
    Bio.Sequence.Block,
    Bio.Sequence.Character,
    Bio.Sequence.Metadata,
    Bio.Graph,
    Bio.Graph.Component,
    Bio.Graph.Constructions,
    Bio.Graph.LeafSet,
    Bio.Graph.Node,
    Bio.Graph.Node.Context,
    Bio.Graph.PhylogeneticDAG,
    Bio.Graph.PhylogeneticDAG.Substitute,
    Bio.Graph.ReferenceDAG,
    Bio.Graph.ReferenceDAG.Internal,
    Bio.Graph.ReferenceDAG.Network,
    Bio.Graph.ReferenceDAG.Traversal,
    Bio.Graph.ReferenceDAG.Utility,
    Bio.Graph.Solution,
    Data.EdgeLength,
    Data.MetricRepresentation,
    Data.NodeLabel,
    Data.TCM,
    Data.TCM.Dense,
    Data.TopologyRepresentation,
    Test.Custom.NucleotideSequence,


-- Normalizing disparate input sources

-- Library for taking data from disperate input sources and converting the data
-- to a normalized form. Normalization does *not* check that the disparate data
-- sources are consistent. That check is performed by the data-unification
-- library.

library data-normalization

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/data-normalization/src

  build-depends:
    alphabet,
    core,
    file-parsers,
    utility,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    graphviz                 >= 2999.20   && < 3000,
    hashable                 >= 1.3       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    semigroupoids            >= 5.3       && < 5.4,
    text-short               >= 0.1.3     && < 1.0,
    vector                   >= 0.12.0.3  && < 0.13,
    vector-instances         >= 3.4       && < 3.5,

  exposed-modules:
    Data.Normalization.Character
    Data.Normalization.Metadata
    Data.Normalization.Topology

  other-modules:
    Data.Normalization.Character.Class
    Data.Normalization.Character.Internal
    Data.Normalization.Metadata.Class
    Data.Normalization.Metadata.Internal


library data-structures

  import:
    ghc-flags,
    language-specs

  visibility:
    private

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/core/data-structures/src

  build-depends:
    alphabet,
    character-name,
    evaluation,
    exportable,
    file-source,
    serialize,
    tcm,
    tcm-memo,
    utility,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 1.0,
    binary                   >= 0.8       && < 1.0,
    binary-instances         >= 1         && < 2.0,
    bv-little                >= 1.0.1     && < 2.0,
    bv-little:instances      >= 1.0.1     && < 2.0,
--    concurrent-hashtable     >= 0.1.8     && < 2.0,
    containers               >= 0.6.2     && < 1.0,
    data-default             >= 0.5.2     && < 0.8,
    deepseq                  >= 1.4       && < 2.0,
    dlist                    >= 0.8       && < 1.0,
    graphviz                 >= 2999.20   && < 3000,
    hashable                 >= 1.3       && < 2.0,
    hashtables               >= 1.2       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    monad-loops              >= 0.4       && < 1.0,
    mono-traversable         >= 1.0       && < 2.0,
    mtl                      >= 2.2.2     && < 3.0,
    parallel                 >= 3.2       && < 4.0,
    pretty-tree              >= 0.1       && < 0.2,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    smallcheck               >= 1.1.5     && < 2.0,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    text-show-instances      >= 3.8       && < 4.0,
    unordered-containers     >= 0.2.10    && < 1.0,
    vector                   >= 0.12.0.3  && < 0.13,
    vector-builder           >= 0.3.7     && < 0.4,
    vector-binary-instances  >= 0.2.1.1   && < 0.3,
    vector-instances         >= 3.4       && < 3.5,
    xml                      >= 1.3.14    && < 1.4,

  exposed-modules:
    Bio.Character
    Bio.Character.Decoration.Additive
    Bio.Character.Decoration.Continuous
    Bio.Character.Decoration.Discrete
    Bio.Character.Decoration.Discrete.Class
    Bio.Character.Decoration.Dynamic
    Bio.Character.Decoration.Fitch
    Bio.Character.Decoration.Metric
    Bio.Character.Decoration.NonMetric
    Bio.Character.Encodable
    Bio.Character.Encodable.Continuous
    Bio.Metadata
    Bio.Metadata.Continuous
    Bio.Metadata.Discrete
    Bio.Metadata.DiscreteWithTCM
    Bio.Metadata.Dynamic
    Bio.Metadata.Metric
    Bio.Metadata.Overlap
    Bio.Sequence
    Bio.Sequence.Block
    Bio.Sequence.Character
    Bio.Sequence.Metadata
    Bio.Graph
    Bio.Graph.Component
    Bio.Graph.Constructions
    Bio.Graph.LeafSet
    Bio.Graph.Node
    Bio.Graph.Node.Context
    Bio.Graph.PhylogeneticDAG
    Bio.Graph.PhylogeneticDAG.Substitute
    Bio.Graph.ReferenceDAG
    Bio.Graph.ReferenceDAG.Internal
    Bio.Graph.ReferenceDAG.Network
    Bio.Graph.ReferenceDAG.Traversal
    Bio.Graph.ReferenceDAG.Utility
    Bio.Graph.Solution
    Data.EdgeLength
    Data.Hashable.Memoize
    Data.NodeLabel
    Data.TopologyRepresentation
    Test.Custom.NucleotideSequence

  other-modules:
    Bio.Character.Decoration.Additive.Class
    Bio.Character.Decoration.Additive.Internal
    Bio.Character.Decoration.Continuous.Class
    Bio.Character.Decoration.Continuous.Internal
    Bio.Character.Decoration.Dynamic.Class
    Bio.Character.Decoration.Dynamic.Internal
    Bio.Character.Decoration.Fitch.Class
    Bio.Character.Decoration.Fitch.Internal
    Bio.Character.Decoration.Metric.Class
    Bio.Character.Decoration.Metric.Internal
    Bio.Character.Decoration.NonMetric.Class
    Bio.Character.Decoration.NonMetric.Internal
    Bio.Character.Decoration.Shared
    Bio.Character.Encodable.Continuous.Class
    Bio.Character.Encodable.Continuous.Internal
    Bio.Character.Encodable.Dynamic
    Bio.Character.Encodable.Dynamic.AmbiguityGroup
    Bio.Character.Encodable.Dynamic.Class
    Bio.Character.Encodable.Dynamic.Element
    Bio.Character.Encodable.Dynamic.Internal
    Bio.Character.Encodable.Internal
    Bio.Character.Encodable.Static
    Bio.Character.Encodable.Static.Class
    Bio.Character.Encodable.Static.Internal
    Bio.Character.Encodable.Stream
    Bio.Metadata.General
    Bio.Metadata.General.Class
    Bio.Metadata.General.Internal
    Bio.Metadata.Discrete.Class
    Bio.Metadata.Discrete.Internal
    Bio.Metadata.DiscreteWithTCM.Class
    Bio.Metadata.DiscreteWithTCM.Internal
    Bio.Metadata.Dynamic.Class
    Bio.Metadata.Dynamic.Internal
    Bio.Metadata.Metric.Class
    Bio.Metadata.Metric.Internal
    Bio.Graph.BinaryRenderingTree
    Bio.Graph.Forest
    Bio.Graph.Node.Internal
    Bio.Graph.PhylogeneticDAG.Internal
    Bio.Graph.PhylogeneticDAG.NetworkEdgeQuantification
    Bio.Graph.PhylogeneticDAG.DynamicCharacterRerooting
    Bio.Graph.PhylogeneticDAG.Postorder
    Bio.Graph.PhylogeneticDAG.Preorder
    Bio.Graph.PhylogeneticDAG.Reification
    Bio.Sequence.Block.Builder
    Bio.Sequence.Block.Character
    Bio.Sequence.Block.Internal
    Bio.Sequence.Block.Metadata
    Bio.Sequence.Internal
    Data.EdgeSet


-- Checking consistentcy and unifiying data from disprate input sources.

-- Library for taking data from disperate input sources and unified the data into
-- a consistent form.

library data-unification

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/data-unification/src

  build-depends:
    character-name,
    core,
    data-normalization,
    file-source,
    serialize,
    utility,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    data-default             >= 0.5.2     && < 0.8,
    deepseq                  >= 1.4       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    mtl                      >= 2.2.2     && < 3.0,
    parallel                 >= 3.2       && < 4.0,
    semigroupoids            >= 5.3       && < 5.4,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    text-show-instances      >= 3.8       && < 4.0,
    transformers             >= 0.5.6     && < 1.0,
    validation               >= 1.1       && < 2.0,

  exposed-modules:
    Data.Unification

  other-modules:
    Data.Unification.Error
    Data.Unification.InputData


-- Monad transformer for managing the stateful evaluation of PCG scripts.

-- This monad transformer provides a concrete, efficient implementation for many
-- useful monad typeclasses required to manage the effects of the evaluating PCG
-- scripts. PCG script evaluation is inherently stateful, requiring management of
-- random seeds, logs, IO, global state variables, and error short-ciruiting.

library evaluation

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/evaluation/src

  build-depends:
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 1.0,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    mtl                      >= 2.2.2     && < 3.0,
    QuickCheck               >= 2.14      && < 3.0,
    quickcheck-instances     >= 0.3.22    && < 1.0,
    semigroupoids            >= 5.3       && < 5.4,
    text                     >= 1.2.4     && < 2.0,
    text-show                >= 3.8.1     && < 4.0,
    transformers             >= 0.5.6     && < 1.0,

  exposed-modules:
    Control.Evaluation
    Control.Monad.Logger
    System.ErrorPhase

  other-modules:
    Control.Evaluation.Notification
    Control.Evaluation.Result
    Control.Evaluation.Trans


library exportable

  import:
    ghc-flags,
    language-specs,

  visibility:
    private

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/exportable/src

  build-depends:
    base                     >= 4.11      && < 5.0,
    lens                     >= 4.18      && < 5.0,
    mono-traversable         >= 1.0       && < 2.0,

  exposed-modules:
    Bio.Character.Exportable

  other-modules:
    Bio.Character.Exportable.Class


-- Parsers for the input file types accepted by PCG

-- Defines file parsers for Fasta files with sequence splitting, Newick files with
-- support for extended newick format, Nexus files (though many block types are
-- skipped), DOT files as graph input, TNT files (though many commands are
-- skipped), and custom file formats for parsing transition cost matrices with an
-- alphabet, and the depricated vertex/edge/root set format.

library file-parsers

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/file-parsers/src

  build-depends:
    alphabet,
    utility,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 1.0,
    case-insensitive         >= 1.2.0     && < 1.3,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    dlist                    >= 0.8       && < 1.0,
    graphviz                 >= 2999.20   && < 3000,
    integer-gmp              >= 1.0       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    matrix                   >= 0.3.6     && < 0.4,
    megaparsec               >= 9.0       && < 10.0,
    mtl                      >= 2.2.2     && < 3.0,
    parser-combinators       >= 1.0       && < 2.0,
    safe                     >= 0.3.17    && < 0.4,
    scientific               >= 0.3.6     && < 0.4,
    semigroupoids            >= 5.3       && < 5.4,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    transformers             >= 0.5.6     && < 1.0,
    vector                   >= 0.12.0.3  && < 0.13,

  exposed-modules:
    File.Format.Dot
    File.Format.Fasta
    File.Format.Fastc
    File.Format.Newick
    File.Format.Nexus
    File.Format.TNT
    File.Format.TransitionCostMatrix
    File.Format.VertexEdgeRoot

  other-modules:
    File.Format.Fasta.Converter
    File.Format.Fasta.Internal
    File.Format.Fasta.Parser
    File.Format.Fastc.Parser
    File.Format.Newick.Internal
    File.Format.Newick.Parser
    File.Format.Nexus.Data
    File.Format.Nexus.Parser
    File.Format.Nexus.Partition
    File.Format.Nexus.Validate
    File.Format.TNT.Command.CCode
    File.Format.TNT.Command.CNames
    File.Format.TNT.Command.Cost
    File.Format.TNT.Command.NStates
    File.Format.TNT.Command.Procedure
    File.Format.TNT.Command.TRead
    File.Format.TNT.Command.XRead
    File.Format.TNT.Parser
    File.Format.TNT.Partitioning
    File.Format.TNT.Internal
    File.Format.TransitionCostMatrix.Parser
    File.Format.VertexEdgeRoot.Parser
    Text.Megaparsec.Custom


-- Library for interacting with file paths in a type-safe manner.

-- Exposes a well-typed file path type and functionality for interacting with the
-- file system through with this type.

library file-source

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/file-source/src

  build-depends:
    serialize,
    validation-transformer,
    base                     >= 4.11      && < 5.0,
    binary                   >= 0.8       && < 1.0,
    bytestring               >= 0.10.10   && < 0.11,
    deepseq                  >= 1.4       && < 2.0,
    directory                >= 1.3.6     && < 2.0,
    filepath                 >= 1.4.2     && < 2.0,
    Glob                     >= 0.9       && < 2.0,
    hashable                 >= 1.3       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    megaparsec               >= 9.0       && < 10.0,
    mono-traversable         >= 1.0       && < 2.0,
    mono-traversable-keys    >= 0.1       && < 1.0,
    pipes                    >= 4.3.10    && < 5.0,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    validation               >= 1.1       && < 2.0,
    vector                   >= 0.12.0.3  && < 0.13,

  exposed-modules:
    Data.FileSource
    Data.FileSource.IO
    Data.FileSource.InputStreamError
    Data.FileSource.ParseStreamError
    Data.FileSource.OutputStreamError


-- Library for phylogenetic graphs

-- Defines data type for phylogenetic graphs and various helper functions.

library graph

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/graph/src

  build-depends:
    utility,
    base                     >= 4.11      && < 5.0,
    comonad                  >= 5.0       && < 6.0,
    containers               >= 0.6.2     && < 0.7,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    mtl                      ^>= 2.2.2            ,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    text-show                >= 3.8.1     && < 4.0,
    vector                   >= 0.12.0.3  && < 0.13,
    vector-builder           >= 0.3.7     && < 0.4,
    vector-instances         >= 3.4       && < 3.5,

  exposed-modules:
    Data.Graph
    Data.Graph.Type
    Data.Graph.Memo
    Data.Graph.Internal
    Data.Graph.Indices
    Data.Graph.NodeContext
    Data.Graph.Moves
    Data.Graph.Hash
    Data.Graph.Intermediate
    Data.Graph.Build
    Data.Graph.Sequence.Class


-- Defines the grammar & specific commands of the PCG language

-- In addition to the grammar definition, this library exports a parser for the
-- grammar. Each command available in PCG is defined here and a script parser is
-- exported to parse a sequence of commands which represents the computation to
-- be performed by PCG.

library language

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/language/src

  build-depends:
    file-source,
    base                     >= 4.11      && < 5.0,
    case-insensitive         >= 1.2.0     && < 1.3,
    containers               >= 0.6.2     && < 1.0,
    free                     >= 5.1       && < 6.0,
    keys                     >= 3.12      && < 4.0,
    megaparsec               >= 9.0       && < 10.0,
    parser-combinators       >= 1.0       && < 2.0,
    scientific               >= 0.3.6     && < 0.4,
    text-short               >= 0.1.3     && < 1.0,
    time                     >= 1.9.3     && < 2.0,

  exposed-modules:
    PCG.Syntax
    PCG.Command.Build
    PCG.Command.Echo
    PCG.Command.Load
    PCG.Command.Read
    PCG.Command.Report
    PCG.Command.Save
    PCG.Command.Version
    PCG.Syntax.Combinators

  other-modules:
    PCG.Syntax.Parser
    PCG.Syntax.Primitive


-- A library for defining custom serialisation and rendering functionality.

-- Defines custom helper functions for serializing to data formats and related
-- to text representations.

library serialize

  import:
    ghc-flags,
    language-specs

  visibility:
    private

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/serialize/src

  build-depends:
    base                     >= 4.11      && < 5.0,
    keys                     >= 3.12      && < 4.0,
    text                     >= 1.2.4     && < 2.0,
    text-show                >= 3.8.1     && < 4.0,
    xml                      >= 1.3.14    && < 1.4,

  exposed-modules:
    TextShow.Custom
    Text.Newick.Class
    Text.XML
    Text.XML.Class
    Text.XML.Custom


-- Library for working with TCMs and SCMs in various representations.

-- General purpose library for working with transition cost matrices (TCMs)
-- and symbol change matrices (SCMs). Specialization options are provided
-- for the discrete metric (non-additive) and the L1 norm (additive) TCMs &
-- SCMs. Exposes a memoized binding for sparsely indexed, large TCMs.

library tcm

  import:
    ffi-buildinfo,
    ghc-flags,
    language-specs,

  visibility:
    private

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/core/tcm/src

  build-depends:
    exportable,
    serialize,
    tcm-memo,
    utility,
    binary                   >= 0.8       && < 1.0,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,
    mono-traversable         >= 1.0       && < 2.0,
    vector                   >= 0.12.0.3  && < 0.13,
    vector-binary-instances  >= 0.2.5     && < 1.0,

  exposed-modules:
    Data.MetricRepresentation
    Data.TCM
    Data.TCM.Dense

  other-modules:
    Data.TCM.Internal
    Data.TCM.Dense.FFI


-- A binding to a C++ hashtable for thread-safe memoization.

-- This package is designed to provide a thread safe binding to a "pure"
-- memoization of two-way and three-way Sankoff character cost and median
-- computations.

library tcm-memo

  import:
    ffi-buildinfo,
    ghc-flags,
    language-specs,

  visibility:
    private

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/tcm-memo/src

  cc-options:       --std=c11

  cxx-options:      --std=c++14

  -- This library is required for linking to the C++ standard template library.
  extra-libraries:  stdc++

  hs-source-dirs:   lib/tcm-memo/ffi

  c-sources:
    lib/tcm-memo/ffi/memoized-tcm/costMatrixWrapper.c
    lib/tcm-memo/ffi/memoized-tcm/dynamicCharacterOperations.c

  cxx-sources:
    lib/tcm-memo/ffi/memoized-tcm/costMatrix_2d.cpp
    lib/tcm-memo/ffi/memoized-tcm/costMatrix_3d.cpp

  -- Here we list all directories that contain C & C++ header files that the FFI
  -- tools will need to locate when preprocessing the C files. Without listing
  -- the directories containing the C header files here, the FFI preprocessor
  -- (hsc2hs, c2hs, etc.) will fail to locate the requisite files. Note also,
  -- that the parent directory of the nessicary C & C++ header files must be
  -- specified. The preprocessor will not recursively look in subdirectories for
  -- header files!
  include-dirs:
    lib/tcm-memo/ffi/memoized-tcm

  build-depends:
    exportable,
    base                     >= 4.11      && < 5.0,
    deepseq                  >= 1.4       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,

  exposed-modules:
    Data.TCM.Memoized
    Data.TCM.Memoized.Types

  other-modules:
    Data.TCM.Memoized.FFI


-- Collection of utility functions and data structures

-- Defines custom data structures for special use cases, more abstract functions
-- that base provides, and re-exported correcting to deficient libraries.

library utility

  import:
    ghc-flags,
    language-specs

  visibility:
    private

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/utility/src

  build-depends:
    base                     >= 4.11      && < 5.0,
    binary                   >= 0.8       && < 1.0,
    bv-little                >= 1.0.1     && < 2.0,
    bv-little:instances      >= 1.0.1     && < 2.0,
    bytestring               >= 0.10.10   && < 0.11,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    foldl                    >= 1.4       && < 2.0,
    hashable                 >= 1.3       && < 2.0,
    integer-gmp              >= 1.0       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    matrix                   >= 0.3.6     && < 0.4,
    mono-traversable         >= 1.0       && < 2.0,
    parallel                 >= 3.2       && < 4.0,
    pointed                  >= 5.0       && < 6.0,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    tasty-hunit              >= 0.10      && < 1.0,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    text-show-instances      >= 3.8       && < 4.0,
    vector                   >= 0.12.0.3  && < 0.13,
    vector-binary-instances  >= 0.2       && < 1.0,
    vector-instances         >= 3.4       && < 3.5,

  exposed-modules:
    Control.Parallel.Custom
    Data.BitMatrix
    Data.Either.Custom
    Data.Foldable.Custom
    Data.List.Utility
    Data.Matrix.NotStupid
    Data.MutualExclusionSet
    Data.Pair.Strict
    Data.Range
    Data.ShortText.Custom
    Data.TextShow.Custom
    Data.UnionSet
    Data.Vector.Custom
    Data.Vector.Memo
    Data.Vector.NonEmpty
    Numeric.Cost
    Numeric.Extended
    Numeric.Extended.Natural
    Numeric.Extended.Real
    Numeric.NonNegativeAverage
    Test.HUnit.Custom
    Test.QuickCheck.Arbitrary.Instances

  other-modules:
    Data.BitMatrix.Internal
    Data.MutualExclusionSet.Internal
    Numeric.Extended.Internal


-- Monad transformer for collecting failures through the applicative instance.

-- This monad transformer provides a data-type for collection failures through
-- the applicative instance. The monad, and monad transformer instances will
-- "short-circuit" with all collected errors at the first monadic bind.

library validation-transformer

  import:
    ghc-flags,
    language-specs

  visibility:
    public

  default-language:
    Haskell2010

  hs-source-dirs:
    lib/validation-transformer/src

  build-depends:
    base                     >= 4.11      && < 5.0,
    deepseq                  >= 1.4       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    transformers             >= 0.5.6     && < 1.0,
    validation               >= 1.1       && < 2.0,

  exposed-modules:
    Control.Monad.Trans.Validation


executable pcg

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    Main.hs

  -- Try to build a static binary, not sure if these flags work
  -- Can also try these flags: -optl-static -optl-pthread
  ghc-options:
    -threaded -rtsopts

  ld-options: -static

  hs-source-dirs:
    app/pcg

  build-depends:
    alphabet,
    character-name,
    evaluation,
    core,
    data-normalization,
    data-unification,
    file-parsers,
    file-source,
    language,
    serialize,
    utility,
    validation-transformer,
    ansi-wl-pprint           >= 0.6.8     && < 0.7,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 2.0,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    filepath                 >= 1.4.2     && < 2.0,
    foldl                    >= 1.4       && < 2.0,
    gitrev                   >= 1.3       && < 2.0,
    graphviz                 >= 2999.20   && < 3000,
    integer-gmp              >= 1.0       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    matrices                 >= 0.5       && < 0.6,
    megaparsec               >= 9.0       && < 10.0,
    mmark                    >= 0.0.7.2   && < 1.0,
    modern-uri               >= 0.3       && < 1.0,
    mono-traversable         >= 1.0       && < 2.0,
    mtl                      >= 2.2.2     && < 3.0,
    optparse-applicative     >= 0.16      && < 1.0,
    parallel                 >= 3.2       && < 4.0,
    perfect-vector-shuffle   >= 0.1       && < 1.0,
    semigroupoids            >= 5.3       && < 5.4,
    template-haskell         >= 2.15      && < 3.0,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    th-lift-instances        >= 0.1       && < 1.0,
    transformers             >= 0.5.6     && < 1.0,
    validation               >= 1.1       && < 2.0,
    vector                   >= 0.12.0.3  && < 0.13,
    xml                      >= 1.3.14    && < 1.4,

  -- Apparently this is needed to compile with profiling, which is really stupid.
  -- It should only be in the modules which use TemplateHaskell, not all modules.
  -- I consider this a temporary hack to get things to compile with profiling.
  -- remove this in the future when cabal gets it's act together.
  other-extensions: TemplateHaskell

  other-modules:
    Paths_phylogenetic_component_graph
    PCG.Command.Build.Evaluate
    PCG.Command.Echo.Evaluate
    PCG.Command.Load.Evaluate
    PCG.Command.Read.DecorationInitialization
    PCG.Command.Read.Evaluate
    PCG.Command.Read.InputStreams
    PCG.Command.Read.ParseStreams
    PCG.Command.Read.ReadCommandError
    PCG.Command.Report.Distance
    PCG.Command.Report.Evaluate
    PCG.Command.Report.GraphViz
    PCG.Command.Report.ImpliedAlignment
    PCG.Command.Report.Metadata
    PCG.Command.Save.Evaluate
    PCG.Command.Version.Evaluate
    PCG.CommandLineOptions
    PCG.CommandLineOptions.Display
    PCG.CommandLineOptions.Types
    PCG.Computation.Internal
    PCG.Software.Credits
    PCG.Software.Metadata


executable 2d-do-comparison

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    2d-do-comparison.hs

  hs-source-dirs:
    app

  build-depends:
    alphabet,
    core,
    tcm-memo,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 2.0,
    mono-traversable         >= 1.0       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,
    tasty                    >= 1.2       && < 2.0,
    tasty-quickcheck         >= 0.9       && < 1.0,


executable 3d-do-comparison

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    3d-do-comparison.hs

  hs-source-dirs:
    app

  build-depends:
    alphabet,
    core,
    tcm-memo,
    base                     >= 4.11      && < 5.0,
--    bimap                    >= 0.3       && < 2.0,
--    deepseq                  >= 1.4       && < 2.0,
--    QuickCheck               >= 2.14      && < 3.0,


executable affine-safety

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    affine-safety.hs

  hs-source-dirs:
    app

  build-depends:
    alphabet,
    core,
    tcm-memo,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,


executable fasta-differ

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    fasta-differ.hs

  hs-source-dirs:
    app

  build-depends:
    file-parsers,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    keys                     >= 3.12      && < 4.0,
    megaparsec               >= 9.0       && < 10.0,
    text-short               >= 0.1.3     && < 1.0,
    vector                   >= 0.12.0.3  && < 0.13,


executable file-tests

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    FileTests.hs

  ghc-options:
    -threaded

  hs-source-dirs:
    lib/file-parsers/test

  build-depends:
    file-parsers,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    directory                >= 1.3.6     && < 2.0,
    megaparsec               >= 9.0       && < 10.0,
    tasty                    >= 1.2       && < 2.0,
    tasty-hunit              >= 0.10      && < 1.0,

  other-modules:
    Test.Custom.Parse
    TestSuite.GeneratedTests
    TestSuite.GeneratedTests.Fasta
    TestSuite.GeneratedTests.Fastc
    TestSuite.GeneratedTests.Internal
    TestSuite.GeneratedTests.Nexus
    TestSuite.GeneratedTests.TNT


executable generate-data-set

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    generate-data-set.hs

  hs-source-dirs:
    app

  build-depends:
    alphabet,
    ansi-wl-pprint           >= 0.6.8     && < 0.7,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    optparse-applicative     >= 0.16      && < 1.0,
    mtl                      >= 2.2.2     && < 3.0,
    mwc-random               >= 0.14      && < 1.0,
    random-shuffle           >= 0.0.4     && < 0.1,
    scientific               >= 0.3.6     && < 0.4,
    text                     >= 1.2.4     && < 2.0,
    text-show                >= 3.8.1     && < 4.0,
    validation               >= 1.1       && < 2.0,
    vector                   >= 0.12.0.3  && < 0.13,


executable generate-random-sequence

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    generate-random-sequence.hs

  hs-source-dirs:
    app

  build-depends:
    alphabet,
    ansi-wl-pprint           >= 0.6.8     && < 0.7,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 1.0,
    bv-little                >= 1.2       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    optparse-applicative     >= 0.16      && < 1.0,
    random                   >= 1.1       && < 2.0,
    scientific               >= 0.3.6     && < 0.4,
    text                     >= 1.2.4     && < 2.0,
    validation               >= 1.1       && < 2.0,


executable generate-tcm

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    generate-tcm.hs

  hs-source-dirs:
    app

  build-depends:
    utility,
    ansi-wl-pprint           >= 0.6.8     && < 0.7,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    keys                     >= 3.12      && < 4.0,
    matrix                   >= 0.3.6     && < 0.4,
    optparse-applicative     >= 0.16      && < 1.0,
    vector                   >= 0.12.0.3  && < 0.13,


executable graphviz-examples

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    graphviz-examples.hs

  hs-source-dirs:
    app/graphviz-examples

  other-modules:
    DisplayTree
    NetworkEdges
    ProjectOverview

  build-depends:
    base                     >= 4.11      && < 5.0,
    directory                >= 1.3.6     && < 1.4,
    filepath                 >= 1.4.2     && < 1.5,
    graphviz                 >= 2999.20   && < 3000,
    text                     >= 1.2.4     && < 1.3,


executable newick-resolver

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    newick-resolver.hs

  hs-source-dirs:
    app

  build-depends:
    file-parsers,
    base                     >= 4.11      && < 5.0,
    megaparsec               >= 9.0       && < 10.0,
    semigroupoids            >= 5.3       && < 5.4,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,


executable reduce-fastc

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    reduce-fastc.hs

  hs-source-dirs:
    app

  build-depends:
    alphabet,
    file-parsers,
    utility,
    ansi-wl-pprint           >= 0.6.8     && < 0.7,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    keys                     >= 3.12      && < 4.0,
    optparse-applicative     >= 0.16      && < 1.0,
    megaparsec               >= 9.0       && < 10.0,
    scientific               >= 0.3.6     && < 0.4,
    semigroupoids            >= 5.3       && < 5.4,
    text-short               >= 0.1.3     && < 1.0,
    validation               >= 1.1       && < 2.0,


executable ukkonen-do-comparison

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    ukkonen-do-comparison.hs

  hs-source-dirs:
    app

  build-depends:
    alphabet,
    core,
    tcm-memo,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,


executable use-the-memoized-ffi

  import:
    ghc-flags,
    language-specs,

  default-language:
    Haskell2010

  main-is:
    Main.hs

  hs-source-dirs:
    lib/tcm-memo

  build-depends:
    exportable,
    tcm-memo,
    base                     >= 4.11      && < 5.0,
    safe                     >= 0.3.17    && < 0.4,


executable integration-tests

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    IntegrationTests.hs

  ghc-options:
    -threaded -with-rtsopts=-N

  hs-source-dirs:
    test

  build-depends:
    core,
    evaluation,
    utility,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 2.0,
    binary                   >= 0.8       && < 1.0,
    bytestring               >= 0.10.10   && < 0.11,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    directory                >= 1.3.6     && < 2.0,
    filepath                 >= 1.4.2     && < 2.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    mtl                      >= 2.2.2     && < 3.0,
    process                  >= 1.6       && < 2.0,
    semigroupoids            >= 5.3       && < 5.4,
    tasty                    >= 1.2       && < 2.0,
    tasty-golden             >= 2.3       && < 3.0,
    tasty-hunit              >= 0.10      && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,
    transformers             >= 0.5.6     && < 1.0,

  other-modules:
    TestSuite.GoldenTests
    TestSuite.ScriptTests
    TestSuite.SubProcess


test-suite test-suite-alphabet

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/alphabet/src
    lib/alphabet/test

  ghc-options:
    -threaded

  build-depends:
    alphabet,
    serialize,
    utility,
    base                     >= 4.11      && < 5.0,
    binary                   >= 0.8       && < 1.0,
    bimap                    >= 0.3       && < 1.0,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    mtl                      >= 2.2.2     && < 3.0,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    tasty                    >= 1.2       && < 2.0,
    tasty-hunit              >= 0.10      && < 1.0,
    tasty-quickcheck         >= 0.9       && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    transformers             >= 0.5.6     && < 1.0,

  other-modules:
    Data.Alphabet.Internal
    Data.Alphabet.Test


test-suite test-suite-analysis

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/core/analysis/test

  build-depends:
    alphabet,
    analysis,
    data-structures,
    tcm,
    tcm-memo,
    utility,
    base                     >= 4.11      && < 5.0,
    clustering               >= 0.4       && < 0.5,
    mono-traversable         >= 1.0       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,
    smallcheck               >= 1.1.5     && < 2.0,
    tasty                    >= 1.2       && < 2.0,
    tasty-quickcheck         >= 0.9       && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,
    tasty-smallcheck         >= 0.8       && < 1.0,
    vector                   >= 0.12.0.3  && < 0.13,

  other-modules:
    Analysis.Parsimony.Dynamic.DirectOptimization.Pairwise.Test
    Analysis.Clustering.Test


test-suite test-suite-data-structures

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  ghc-options:
    -threaded
--    -with-rtsopts=-N4

  hs-source-dirs:
    lib/core/data-structures/test

  build-depends:
    alphabet,
    data-structures,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
--    deepseq                  >= 1.4       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    mono-traversable         >= 1.0       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,
    tasty                    >= 1.2       && < 2.0,
    tasty-hunit              >= 0.10      && < 1.0,
    tasty-quickcheck         >= 0.9       && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,
    vector                   >= 0.12.0.3  && < 0.13,

  other-modules:
    Bio.Character.Encodable.Dynamic.Test
    Bio.Character.Encodable.Static.Test
    Bio.Graph.ReferenceDAG.Test
    Bio.Graph.ReferenceDAG.Test.NetworkPropertyTests
    Bio.Graph.ReferenceDAG.Test.NetworkUnitTests


test-suite test-suite-evaluation

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/evaluation/test

  ghc-options:
    -threaded

  build-depends:
    evaluation,
    base                     >= 4.11      && < 5.0,
    deepseq                  >= 1.4       && < 2.0,
    mtl                      >= 2.2.2     && < 3.0,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    transformers             >= 0.5.6     && < 1.0,
    tasty                    >= 1.2       && < 2.0,
    tasty-quickcheck         >= 0.9       && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,

  other-modules:
    Control.Evaluation.Test
    System.ErrorPhase.Test


test-suite test-suite-file-parsers

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/file-parsers/src,
    lib/file-parsers/test

  ghc-options:
    -threaded

  build-depends:
    alphabet,
    utility,
    base                     >= 4.11      && < 5.0,
    bimap                    >= 0.3       && < 1.0,
    case-insensitive         >= 1.2.0     && < 1.3,
    containers               >= 0.6.2     && < 1.0,
    deepseq                  >= 1.4       && < 2.0,
    dlist                    >= 0.8       && < 1.0,
    integer-gmp              >= 1.0       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    matrix                   >= 0.3.6     && < 0.4,
    megaparsec               >= 9.0       && < 10.0,
    parser-combinators       >= 1.0       && < 2.0,
    QuickCheck               >= 2.14      && < 3.0,
    safe                     >= 0.3.17    && < 0.4,
    scientific               >= 0.3.6     && < 0.4,
    semigroupoids            >= 5.3       && < 5.4,
    smallcheck               >= 1.1.5     && < 2.0,
    tasty                    >= 1.2       && < 2.0,
    tasty-hunit              >= 0.10      && < 1.0,
    tasty-quickcheck         >= 0.9       && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    vector                   >= 0.12.0.3  && < 0.13,

  other-modules:
    File.Format.Fasta.Internal
    File.Format.Fasta.Parser
    File.Format.Fasta.Test
    File.Format.Fastc.Parser
    File.Format.Fastc.Test
    File.Format.Newick.Internal
    File.Format.Newick.Parser
    File.Format.Newick.Test
    File.Format.TNT.Command.CCode
    File.Format.TNT.Command.CNames
    File.Format.TNT.Command.Procedure
    File.Format.TNT.Command.TRead
    File.Format.TNT.Command.XRead
    File.Format.TNT.Internal
    File.Format.TNT.Test
    File.Format.TransitionCostMatrix.Parser
    File.Format.TransitionCostMatrix.Test
    File.Format.VertexEdgeRoot.Parser
    File.Format.VertexEdgeRoot.Test
    Test.Custom.Parse
    Text.Megaparsec.Custom
    Text.Megaparsec.Custom.Test


test-suite test-suite-graph

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/graph/test

  build-depends:
    graph,
    utility,
    base                     >= 4.11      && < 5.0,
    tasty                    >= 1.2       && < 2.0,
    vector                   >= 0.12.0.3  && < 0.13,

  other-modules:
    Data.Graph.Test


test-suite test-suite-tcm

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/core/tcm/test

  ghc-options:
    -threaded

  build-depends:
    tcm,
    tcm-memo,
    utility,
    base                     >= 4.11      && < 5.0,
    mono-traversable         >= 1.0       && < 2.0,
    tasty                    >= 1.2       && < 2.0,
    tasty-hunit              >= 0.10      && < 1.0,
    tasty-quickcheck         >= 0.9       && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,

  other-modules:
    Data.TCM.Test


test-suite test-suite-utility

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/utility/test

  build-depends:
    utility,
    base                     >= 4.11      && < 5.0,
    bv-little                >= 1.0.1     && < 2.0,
    bv-little:instances      >= 1.0.1     && < 2.0,
    deepseq                  >= 1.4       && < 2.0,
    integer-gmp              >= 1.0       && < 2.0,
    mono-traversable         >= 1.0       && < 2.0,
    parallel                 >= 3.2       && < 4.0,
    QuickCheck               >= 2.14      && < 3.0,
    tasty                    >= 1.2       && < 2.0,
    tasty-hunit              >= 0.10      && < 1.0,
    tasty-quickcheck         >= 0.9       && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,

  other-modules:
    Control.Parallel.Test
    Data.BitMatrix.Test
    Data.MutualExclusionSet.Test
    Numeric.Cost.Test
    Numeric.Extended.Natural.Test
    Numeric.Extended.Real.Test
    Numeric.NonNegativeAverage.Test
    Data.List.Test


test-suite test-suite-validation-transformer

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    TestSuite.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/validation-transformer/test

  ghc-options:
    -threaded

  build-depends:
    validation-transformer,
    base                     >= 4.11      && < 5.0,
    deepseq                  >= 1.4       && < 2.0,
    mtl                      >= 2.2.2     && < 3.0,
    QuickCheck               >= 2.14      && < 3.0,
    semigroupoids            >= 5.3       && < 5.4,
    tasty                    >= 1.2       && < 2.0,
    tasty-quickcheck         >= 0.9       && < 1.0,
    tasty-rerun              >= 1.1.14    && < 2.0,
    transformers             >= 0.5.6     && < 1.0,

  other-modules:
    Control.Monad.Trans.Validation.Test


benchmark bench-file-parsers-space

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    Space.hs

  type:
    exitcode-stdio-1.0

  ghc-options:
    -threaded

  hs-source-dirs:
    lib/file-parsers/bench

  build-depends:
    file-parsers,
    base                     >= 4.11      && < 5.0,
    case-insensitive         >= 1.2.0     && < 1.3,
    criterion                >= 1.5       && < 2.0,
    deepseq                  >= 1.4       && < 2.0,
    filepath                 >= 1.4.2     && < 2.0,
    megaparsec               >= 9.0       && < 10.0,
    text                     >= 1.2.4     && < 2.0,
    weigh                    >= 0.0.16    && < 1.0,

  other-modules:
    Benchmark.FASTA.Files
    Benchmark.FASTA.Space
    Benchmark.FASTC.Files
    Benchmark.FASTC.Space
    Benchmark.Internal
    Benchmark.Newick.Files
    Benchmark.Newick.Space
    Benchmark.TCM.Files
    Benchmark.TCM.Space
    Benchmark.VER.Files
    Benchmark.VER.Space


benchmark bench-file-parsers-time

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    Time.hs

  type:
    exitcode-stdio-1.0

  ghc-options:
    -threaded

  hs-source-dirs:
    lib/file-parsers/bench

  build-depends:
    file-parsers,
    base                     >= 4.11      && < 5.0,
    case-insensitive         >= 1.2.0     && < 1.3,
    criterion                >= 1.5       && < 2.0,
    deepseq                  >= 1.4       && < 2.0,
    filepath                 >= 1.4.2     && < 2.0,
    megaparsec               >= 9.0       && < 10.0,
    text                     >= 1.2.4     && < 2.0,
    weigh                    >= 0.0.16    && < 1.0,

  other-modules:
    Benchmark.FASTA.Files
    Benchmark.FASTA.Time
    Benchmark.FASTC.Files
    Benchmark.FASTC.Time
    Benchmark.Internal
    Benchmark.Newick.Files
    Benchmark.Newick.Time
    Benchmark.TCM.Files
    Benchmark.TCM.Time
    Benchmark.VER.Files
    Benchmark.VER.Time


benchmark bench-graph

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    Benchmarks.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/graph/bench

  build-depends:
    utility,
    base                     >= 4.11      && < 5.0,

  ghc-options:
    -fdicts-cheap
    -fmax-simplifier-iterations=10
    -fspec-constr-count=6
    -threaded

  other-modules:
    Data.Graph.Bench


benchmark bench-string-alignment

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    bench-string-alignment.hs

  type:
    exitcode-stdio-1.0

  ghc-options:
    -threaded
    -rtsopts
    -with-rtsopts=-N

  build-depends:
    alphabet,
    core,
    data-normalization,
    data-unification,
    evaluation,
    file-parsers,
    file-source,
    language,
    utility,
    validation-transformer,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    criterion                >= 1.5       && < 2.0,
    deepseq                  >= 1.4       && < 2.0,
    directory                >= 1.3.6     && < 1.4,
    filepath                 >= 1.4.2     && < 2.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    megaparsec               >= 9.0       && < 10.0,
    mono-traversable         >= 1.0       && < 2.0,
    parallel                 >= 3.2       && < 4.0,
    semigroupoids            >= 5.3       && < 5.4,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    validation               >= 1.1       && < 2.0,
    vector                   >= 0.12.0.3  && < 0.13,

  hs-source-dirs:
    app/pcg
    bench

  other-modules:
    Benchmark.StringAlignment
    PCG.Command.Read.InputStreams
    PCG.Command.Read.ParseStreams
    PCG.Command.Read.ReadCommandError


benchmark bench-string-alignment-small

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    bench-string-alignment-small.hs

  type:
    exitcode-stdio-1.0

  ghc-options:
    -threaded
    -rtsopts
    -with-rtsopts=-N

  build-depends:
    alphabet,
    core,
    data-normalization,
    data-unification,
    evaluation,
    file-parsers,
    file-source,
    language,
    utility,
    validation-transformer,
    base                     >= 4.11      && < 5.0,
    containers               >= 0.6.2     && < 1.0,
    criterion                >= 1.5       && < 2.0,
    deepseq                  >= 1.4       && < 2.0,
    directory                >= 1.3.6     && < 1.4,
    filepath                 >= 1.4.2     && < 2.0,
    keys                     >= 3.12      && < 4.0,
    lens                     >= 4.18      && < 5.0,
    megaparsec               >= 9.0       && < 10.0,
    mono-traversable         >= 1.0       && < 2.0,
    parallel                 >= 3.2       && < 4.0,
    semigroupoids            >= 5.3       && < 5.4,
    text                     >= 1.2.4     && < 2.0,
    text-short               >= 0.1.3     && < 1.0,
    text-show                >= 3.8.1     && < 4.0,
    validation               >= 1.1       && < 2.0,
    vector                   >= 0.12.0.3  && < 0.13,

  hs-source-dirs:
    app/pcg
    bench

  other-modules:
    Benchmark.StringAlignment
    PCG.Command.Read.InputStreams
    PCG.Command.Read.ParseStreams
    PCG.Command.Read.ReadCommandError


benchmark bench-new-str-align

  import:
    ghc-flags,
    language-specs

  default-language: Haskell2010

  main-is:
    bench-new-str-align.hs

  type:
    exitcode-stdio-1.0

  ghc-options:
    -rtsopts
    -with-rtsopts=-T

  build-depends:
    alphabet,
    core,
    data-normalization,
    data-unification,
    evaluation,
    file-parsers,
    file-source,
    language,
    tcm-memo,
    utility,
    validation-transformer,
    base                     >= 4.11      && < 5.0,
    bench-show               >= 0.3       && < 1.0,
    containers               >= 0.6.2     && < 1.0,
    criterion                >= 1.5       && < 2.0,
    deepseq                  >= 1.4       && < 2.0,
    keys                     >= 3.12      && < 4.0,
    megaparsec               >= 9.0       && < 10.0,
    split                    >= 0.2       && < 1.0,
    text-short               >= 0.1.3     && < 1.0,
    weigh                    >= 0.0.16    && < 1.0,

  hs-source-dirs:
    bench


benchmark bench-utility

  import:
    ghc-flags,
    language-specs

  default-language:
    Haskell2010

  main-is:
    Benchmarks.hs

  type:
    exitcode-stdio-1.0

  hs-source-dirs:
    lib/utility/bench

  build-depends:
    utility,
    base                     >= 4.11      && < 5.0,
    criterion                >= 1.5       && < 2.0,
    deepseq                  >= 1.4       && < 2.0,

  ghc-options:
    -fdicts-cheap
    -fmax-simplifier-iterations=10
    -fspec-constr-count=6
    -threaded

  other-modules:
    Data.MutualExclusionSet.Bench
    Numeric.Extended.Natural.Bench
|]
