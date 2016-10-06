//
//  Signature.swift
//  SwiftCheck
//
//  Created by Robert Widmann on 9/22/16.
//  Copyright © 2016 Typelift. All rights reserved.
//

/*
testable :: Typeable a => Sig -> a -> Bool
testable sig x =
  typeOf x `Map.member` observers sig ||
  typeOf x `Map.member` ords sig

-- Given a constant, find the types of its partial applications.
constantApplications :: forall a. Typeable a => Sig -> Constant a -> [Witness]
constantApplications sig (Constant (Atom {sym = sym })) =
  map (findWitness sig)
    (take (symbolArity sym + 1)
     (iterate rightArrow (typeOf (undefined :: a))))

-- Find the argument types of a constant.
constantArgs :: forall a. Typeable a => Sig -> Constant a -> [Witness]
constantArgs sig (Constant (Atom { sym = sym })) =
  map (findWitness sig)
    (take (symbolArity sym)
     (unfoldr splitArrow (typeOf (undefined :: a))))

-- Find the type of a saturated constant.
constantRes :: forall a. Typeable a => Sig -> Constant a -> Witness
constantRes sig (Constant (Atom { sym = sym })) =
  findWitness sig
    (iterate (snd . fromMaybe (ERROR msg) . splitArrow)
       (typeOf (undefined :: a)) !! symbolArity sym)
  where msg = "constantRes: type oversaturated"

-- The set of types returned by saturated constants.
saturatedTypes :: Sig -> [Witness]
saturatedTypes sig =
  usort
    [ constantRes sig k
    | Some k <- TypeRel.toList (constants sig) ]

-- The set of types of which there is a non-variable term.
inhabitedTypes :: Sig -> [Witness]
inhabitedTypes sig =
  usort . concat $
    [ constantApplications sig k
    | Some k <- TypeRel.toList (constants sig) ]

-- The set of types that appear as arguments to functions.
argumentTypes :: Sig -> [Witness]
argumentTypes sig =
  usort . concat $
    [ constantArgs sig k
    | Some k <- TypeRel.toList (constants sig) ]

-- The set of types inhabited by variables.
variableTypes :: Sig -> [Witness]
variableTypes sig =
  usort (map someWitness (TypeRel.toList (variables sig)))

*/
