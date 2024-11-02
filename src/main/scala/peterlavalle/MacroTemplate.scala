package peterlavalle

import java.io.File
import scala.annotation.tailrec
import scala.io.Source
import scala.quoted.*

trait MacroTemplate {

	inline def template(extension: String): LazyList[String] =
		${ MacroTemplate.template('{ extension }) }

	inline def template: LazyList[String] =
		${ MacroTemplate.template('{ "" }) }

}

object MacroTemplate {

	def template(extension: Expr[String])(using quotes: Quotes): Expr[LazyList[String]] =

		import quotes.*

		val scalaFile =
			File({
				import quotes.reflect.*
				Position.ofMacroExpansion.sourceFile.jpath.toString
			}).getAbsoluteFile

		if !scalaFile.isFile then throw MissingScalaException(scalaFile)

		val template =

			val extensionValue = extension.valueOrError

			val srcFolder = scalaFile.getParentFile.getAbsoluteFile
			val className = getEnclosingClassName
			val methodName = getEnclosingMethodName

			if ("" == extensionValue)
				File(srcFolder, s"$className.$methodName")
			else
				File(srcFolder, s"$className.$methodName.$extensionValue")

		if !template.isFile then throw MissingTemplateException(scalaFile, template)

		// get the textual source
		val source =
			val src = Source.fromFile(template)
			try
				src.mkString
					.split("[\r \t]*\n").toList
			finally
				src.close()

		// glue the source back together
		val eSource =
			Expr {
				source
					.foldLeft("")(_ + _ + "\n")
			}


		// get all the names used in the template
		val names = {
			@tailrec
			def names(todo: List[String], done: Set[String]): Set[String] =
				todo match
					case Nil => done
					case ResourceTemplate.rValue(_, _, key, aft) :: tail =>
						names(aft :: tail, done + key)

					case _ :: tail =>
						names(tail, done)

			names(source, Set())
		}

		'{
			ResourceTemplate.bind($eSource, ${
				names
					.foldLeft {
						val collect: Expr[ResourceTemplate.Values] = '{
							(s: String) =>
								throw Exception(s"value $s was unhandled - this shouldn't happen as we did compile time checks")
						}
						collect
					} {
						case (left, key) =>
							val k = Expr(key)

							val a: Option[Expr[Any]] = {

								// Get the names of the variables passed as arguments to the enclosing method
								val enclosingMethodParameters: List[(String, Any)] = {
									import quotes.reflect.*

									// Start from the macro expansion's enclosing symbol
									@tailrec
									def findEnclosingMethod(sym: Symbol): Option[Symbol] =
										if sym.isDefDef then Some(sym)
										else if sym == Symbol.noSymbol then None
										else findEnclosingMethod(sym.owner)

									val enclosingMethodOpt = findEnclosingMethod(Symbol.spliceOwner)

									enclosingMethodOpt match {
										case Some(methodSymbol) =>
											// Retrieve the parameter list for the enclosing method
											methodSymbol.paramSymss.flatten.collect {
												case param if param.isTerm && param.isValDef =>
													// Get parameter name and create a reference to access it
													val paramName = param.name
													val paramTerm = Ref(param).asExprOf[Any]
													(paramName, paramTerm)
											}
										case None =>
											List.empty
									}
								}

								enclosingMethodParameters
									.find(_._1 == key)
									.map(_._2.asInstanceOf[Expr[Any]])
							} // scan args

							val r: Option[Expr[Any]] = {

								// ...
								def resolve(key: String): Option[Expr[Any]] = {
									import quotes.reflect.*

									// Retrieve the symbol of the method or class where the macro is invoked
									val symbol = Symbol.spliceOwner

									// Helper function to find the first matching symbol in the enclosing scope
									def findInScope(symbol: Symbol): Option[Symbol] = {
										// Look for fields, parameters, or methods in the owner
										val candidates =
											symbol.memberFields.find(_.name == key).orElse(
												symbol.memberMethods.find(_.name == key)
											)

										// If the key was not found in the current symbol, continue up the chain
										candidates.orElse(if symbol.owner != Symbol.noSymbol then findInScope(symbol.owner) else None)
									}

									// Use `findInScope` to get the symbol corresponding to `key`
									findInScope(symbol) match {
										case Some(candidate) if candidate.isTerm && candidate.exists =>
											// Create a reference to this symbol
											Some(Ref(candidate).asExprOf[Any])

										case _ => None // Return None if no match is found
									}
								}


								resolve(key)
							}

							'{
								(s: String) =>
									if (s.equals($k.toString))
										${
											a.orElse(r)
												.getOrElse {
													throw MissingKeyException(key, template)
												}
												.asInstanceOf[Expr[ResourceTemplate.Bound]]
										}
									else
										$left(s)
							}
					}
			})
		}


	private def getEnclosingClassName(using quotes: Quotes): String =
		import quotes.reflect.*
		// Recursively traverse owner chain to find the nearest class definition
		def findClass(symbol: Symbol): Option[Symbol] =
			if symbol.isClassDef then Some(symbol)
			else if symbol == Symbol.noSymbol then None
			else findClass(symbol.owner)

		findClass(Symbol.spliceOwner).map(_.name).getOrElse("UnknownClass")

	private def getEnclosingMethodName(using quotes: Quotes): String =
		import quotes.reflect.*
		// Recursively traverse owner chain to find the nearest method definition
		def findMethod(symbol: Symbol): Option[Symbol] =
			if symbol.isDefDef then Some(symbol)
			else if symbol == Symbol.noSymbol then None
			else findMethod(symbol.owner)

		findMethod(Symbol.spliceOwner).map(_.name).getOrElse("UnknownMethod")

	class MissingKeyException(key: String, src: File) extends Exception(
		s"couldn't find value/data for key `$key` be sure it's a parameter or field or something or remove it from " + src.getAbsolutePath
	)

	class MissingScalaException private[MacroTemplate](path: File) extends Exception(
		s"the scala source file `${path.getAbsolutePath}` isn't present"
	)

	class MissingTemplateException private[MacroTemplate](code: File, path: File) extends Exception(
		s"the textual source file `${path.getAbsolutePath}` isn't present\n\t\t... next to ${code.getAbsolutePath}"
	)

}