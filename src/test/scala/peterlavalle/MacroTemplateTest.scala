package peterlavalle

class MacroTemplateTest extends munit.FunSuite {

	test("so now what") {

		class Donk extends MacroTemplate {
			def bar() = template

			def foo = 8
			

			def bar(name: String) = template("txt")
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
