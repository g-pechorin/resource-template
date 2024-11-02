package peterlavalle

import java.util
import scala.io.Source
import scala.util.matching.Regex


trait ResourceTemplate {

	import ResourceTemplate.*

	def just(name: String): String =
		bind(name) {
			(what: String) =>
				sys.error(s"wasn't expecting a key in this template, but found $what")
		}.foldLeft("")(_ + _ + "\n")

	def bind(name: String): Values => LazyList[String] =
		val stream = {
			val src = {

				getClass.getSimpleName
					.reverse.dropWhile('$' == _)
					.reverse
			} + "." + name
			val stream = getClass.getResourceAsStream(src)

			require(
				null != stream,
				s"didn't find resource `$src`"
			)

			stream
		}
		val src = Source.fromInputStream(stream)

		try
			val todo = src.mkString
			(bind: Values) =>
				ResourceTemplate.bind(todo, bind)
		finally
			src.close()
}

object ResourceTemplate:
	type Bound = String | Iterable[?] | AnyVal
	type Values = String => Bound
	val rValue: Regex = """(.*?)(<#(;?\w+)#>(.*))""".r

	def bind(todo: String, data: String => Bound): LazyList[String] =
		val cache = util.HashMap[String, LazyList[String]]()

		ResourceTemplate.fill(todo.split("[\r \t]*\n")
			.to(LazyList), key => {

			def depak: Bound => LazyList[String] = {
				case text: String =>
					LazyList(text)
				case nil: Seq[_] if nil.isEmpty =>
					LazyList()
				case v: AnyVal =>
					LazyList(v.toString)
				case i: Iterable[?] =>
					i.to(LazyList)
						.flatMap {
							case b: Bound =>
								depak(b)
						}
			}

			if (!cache.containsKey(key)) {
				val bound = data(key)
				require(null != bound, s"null == bind($key) when looking up a template value")
				val dep = depak(bound)
				require(null != dep)
				cache.put(key, dep)
			}
			cache.get(key)
		})


	def fill(todo: LazyList[String], data: String => LazyList[String]): LazyList[String] =
		todo match
			case rValue(pre, _, key, aft) #:: tail =>
				fill(
					(if (key.startsWith(";")) LazyList() else data(key)).to(LazyList).map {
						data =>
							pre + data + aft
					} ++ tail,
					data)
			case head #:: tail =>
				head #:: fill(tail, data)
			case LazyList() =>
				LazyList()

