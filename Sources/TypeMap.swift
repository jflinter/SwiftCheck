//
//  TypeMap.swift
//  SwiftCheck
//
//  Created by Robert Widmann on 9/22/16.
//  Copyright © 2016 Typelift. All rights reserved.
//

struct Witnessed<A> {
	let witness : A
}

struct Tagged<A> {
	let tag : Witnessed<A>
	let typeRep : TypeRep
	let erase : A
}

struct GenTypeMap {
	let unMap : Dictionary<TypeRep, Gen<Any>>

	init() {
		self.unMap = [:]
	}

	init<A : Typeable>(_ x : Gen<A>) {
		self.unMap = [ A.type: x.map({ $0 as Any }) ]
	}

	func lookup<A : Typeable>(_ def : Gen<A>, _ x : A) -> Gen<A> {
		switch self.unMap[A.type] {
		case .none:
			return def
		case let .some(y):
			return y.map { $0 as! A }
		}
	}
}

struct ConstantTypeRel {
	let unMap : Dictionary<TypeRep, [Constant<Any>]>

	init() {
		self.unMap = [:]
	}

	init<A : Typeable>(_ x : Constant<A>) {
		self.unMap = [ A.type: [x].map({ $0.upcast }) ]
	}

	func lookup<A : Typeable>(_ def : Constant<A>, _ x : A) -> [Constant<A>] {
		switch self.unMap[A.type] {
		case .none:
			return [def]
		case let .some(y):
			return y.map { $0 as! Constant<A> }
		}
	}
}

struct VariableTypeRel {
	let unMap : Dictionary<TypeRep, [Variable<Any>]>

	init() {
		self.unMap = [:]
	}

	init<A : Typeable>(_ x : Variable<A>) {
		self.unMap = [ A.type: [x].map({ $0.upcast }) ]
	}

	func lookup<A : Typeable>(_ def : Variable<A>, _ x : A) -> [Variable<A>] {
		switch self.unMap[A.type] {
		case .none:
			return [def]
		case let .some(y):
			return y.map { $0 as! Variable<A> }
		}
	}
}

struct ExprTypeRel {
	let unMap : Dictionary<TypeRep, [Expr<Any>]>

	init() {
		self.unMap = [:]
	}

	init<A : Typeable>(_ x : Expr<A>) {
		self.unMap = [ A.type: [x].map({ $0.upcast }) ]
	}

	func lookup<A : Typeable>(_ def : Expr<A>, _ x : A) -> [Expr<A>] {
		switch self.unMap[A.type] {
		case .none:
			return [def]
		case let .some(y):
			return y.map { $0 as! Expr<A> }
		}
	}
}


