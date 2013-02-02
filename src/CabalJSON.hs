{-# LANGUAGE FlexibleInstances, UndecidableInstances, TemplateHaskell, DefaultSignatures, DeriveGeneric, StandaloneDeriving #-}

-- | A quick and dirty instance for 'FromJSON GenericPackageDescription',
-- created with template toolkit magic
module CabalJSON where

import Data.Aeson.TH
import Distribution.Text
import Distribution.System
import Distribution.License
import Distribution.Compiler
import Distribution.Version
import Distribution.Package
import Distribution.PackageDescription
import Distribution.ModuleName
import Language.Haskell.Extension
import Data.Aeson

-- The derived instance is just too ugly here
instance ToJSON Version where
    toJSON = toJSON . display

$(deriveToJSON id ''Language)
$(deriveToJSON id ''KnownExtension)
$(deriveToJSON id ''Extension)
$(deriveToJSON id ''TestType)
$(deriveToJSON id ''ModuleName)
$(deriveToJSON id ''PackageName)
$(deriveToJSON id ''BenchmarkType)
$(deriveToJSON id ''RepoKind)
$(deriveToJSON id ''RepoType)
$(deriveToJSON id ''BuildInfo)
$(deriveToJSON id ''TestSuiteInterface)
$(deriveToJSON id ''BenchmarkInterface)
$(deriveToJSON id ''PackageIdentifier)
$(deriveToJSON id ''License)
$(deriveToJSON id ''CompilerFlavor)
$(deriveToJSON id ''VersionRange)
$(deriveToJSON id ''SourceRepo)
$(deriveToJSON id ''Dependency)
$(deriveToJSON id ''BuildType)
$(deriveToJSON id ''Library)
$(deriveToJSON id ''Executable)
$(deriveToJSON id ''TestSuite)
$(deriveToJSON id ''Benchmark)
$(deriveToJSON id ''PackageDescription)
$(deriveToJSON id ''FlagName)
$(deriveToJSON id ''OS)
$(deriveToJSON id ''Arch)
$(deriveToJSON id ''Condition)
$(deriveToJSON id ''Flag)
$(deriveToJSON id ''ConfVar)
$(deriveToJSON id ''CondTree)
$(deriveToJSON id ''GenericPackageDescription)
-- $(deriveToJSON id ''Version)
