package peterlavalle

class MacroTemplateTest extends munit.FunSuite {

	test("so now what") {

		class Donk extends ResourceTemplate.Macro {
			def bar() =
				// with no extension this reads "Donk.bar"as the template source
				// https://github.com/g-pechorin/resource-template/blob/default/src/test/scala/peterlavalle/Donk.bar
				template

			def foo = 8
			

			def bar(name: String) =
				// this one uses a different extension ... so i can have lots of matching methods for reasons
				// ths file is at https://github.com/g-pechorin/resource-template/blob/default/src/test/scala/peterlavalle/Donk.bar.txt
				template("txt")
		}

		assertEquals(
			new Donk().bar().toList,
			List(
				"",
				"this is the no args donk",
				"",
				"he haw"
			)
		)
		assertEquals(
			new Donk().bar("fred").toList,
			List(
				"okay then 8 what have you fred got to say?"
			)
		)
	}
}
