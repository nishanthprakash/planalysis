import ast as A
import parse-pyret as SP
import file as F

gtest =   ```
          data Report:
  | max-hz(day :: Number, max-reading :: Number)
          end

          fun fxtxexsxt():
          5
          where:
          fxtxexsxt([list: 20140901, 101.1, 100, 120, 20140902, 500, 450, 100, 
    20140904, 90, 300, 299.9, 20140905, 400, 20141001, 20, 20141002, 400, 
    500, 300, 20141101, 30], 10) is [list: max-hz(1, 20), max-hz(2, 500)]
          fxtxexsxt([list: 20151004, 200, 150, 175]) is max-hz(4, 200)
          end
          ```

#p = SP.surface-parse(F.input-file("test-file.arr").read-file(), "test-file.arr")


p = SP.surface-parse(
  ```
  provide *

  data Report:
  | max-hz(day :: Number, max-reading :: Number)
  end

  test-nums-2 = [list: 20150101, 100.1, 97, 20150102, 10, 75, 200.4, 
  20150115, 309, 407, 20150301, 10 ]

  fun daily-max-for-month(sensor-data :: List<Number>, month :: Number) -> List<Report>:
  fun parse(l :: List<Number>, cur-month :: Number, 
      cur-report :: List<Report>) -> List<Report>:
    cases(List) l:
      |empty => cur-report
      |link(f,r) => 
        if f > 500:
          check-month = string-to-number(string-substring(
              num-to-string(f), 4,  6)).value
          if check-month == month:
            parsed-day = string-to-number(string-substring(
                num-to-string(f), 6,  8)).value
            parse(r, check-month, link(max-hz(parsed-day, 0), cur-report))
          else:
            parse(r, check-month, cur-report)
          end
        else if (f <= 500) and (cur-month == month):
          if f > cur-report.first.max-reading:
            parse(r, cur-month, link(max-hz(cur-report.first.day, f), 
                cur-report.rest))
          else:
            parse(r, cur-month, cur-report)
          end
        else:
          parse(r, cur-month, cur-report)
        end
    end
  end
  parse(sensor-data, month, empty).reverse()         
  where:
  daily-max-for-month(test-nums-2, 3) is [list: max-hz(1, 10)]
  daily-max-for-month(test-nums-2, 2) is empty
  end
  ```, 
  "earthquake-2")

fun replace-where(wblock, fname):
  var whrblock = none
  whast = SP.surface-parse(string-replace(gtest,"fxtxexsxt", fname), "name-of-program.arr")
  get-where = A.default-map-visitor.{
    method s-fun(self, l, name, params, args, ann, doc, body, where-block, blocky): 
      A.s-fun(
        l,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        block:
          whrblock := where-block
          where-block
        end,
        blocky)
    end
  }
  wast = whast.visit(get-where)
  whrblock
end

var function-counter = 0

modify-functions = A.default-map-visitor.{
  method s-fun(self, l, name, params, args, ann, doc, body, where-block, blocky):
    A.s-fun(
      l,
      name,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      doc,
      body.visit(self),
      replace-where(where-block, name),
      blocky)
  end,

  method s-app(self, l, _fun, args):
    #s-app(l, _fun.visit(self), args.map(_.visit(self)))
    block:
      function-counter := function-counter + 1

      f-str = "f" + num-to-string(function-counter)
      fs-len = string-length(f-str)

      let-args = args.foldr(
        lam(x, y):
          cases(List) y:
            | empty => link(A.s-let(l, A.s-bind(l, false, A.s-name(l,  "_" + f-str + "__in_1"), A.a-blank), x.visit(self), false), y)
            | link(first, rest) => 
              in-index = string-to-number(string-substring(first.name.id.s, 6 + fs-len, string-length(first.name.id.s))).value + 1
              arg-name = "_" + f-str + "__in_" + num-to-string(in-index)
              link(A.s-let(l, A.s-bind(l, false, A.s-name(l, arg-name), A.a-blank), x.visit(self), false), y)  
          end
        end, empty)

      list-argids = args.foldr(
        lam(_, y):
          cases(List) y:
            | empty => link(A.s-id(l, A.s-name(l, "_" + f-str + "__in_1")), y)
            | link(first, rest) => 
              in-index = string-to-number(string-substring(first.id.s, 6 + fs-len, string-length(first.id.s))).value + 1
              arg-name = "_" + f-str + "__in_" + num-to-string(in-index)
              link(A.s-id(l, A.s-name(l, arg-name)), y)  
          end
        end, empty)


      in-and-out-rev = (link( 
          A.s-let(l, 
            A.s-bind(l, false, A.s-name(l, "_" + f-str + "__out"), A.a-blank), 
            A.s-app(l, 
              _fun.visit(self), 
              list-argids), false), 
          let-args.reverse()))

      add-prints-rev = 	link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-str(l, '"}},')]),
		      				link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-id(l, A.s-name(l, "_" + f-str + "__out"))]), 
			      				link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-str(l, '", "ouput":"')]), 
				      				link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-construct(l, A.s-construct-normal, A.s-id(l, A.s-name(l, "list")), list-argids)]), 
				      					link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-str(l, '{"' + f-str + '":{"input":"')]), 
				      						in-and-out-rev)))))

      block-out = A.s-id(l, A.s-name(l, "_" + f-str + "__out"))

      anf-complete = (link(block-out, add-prints-rev).reverse())

      A.s-user-block(l, A.s-block(l, anf-complete))
    end
  end
}

modified = p.visit(modify-functions)

as-string = modified.tosource().pretty(80).join-str("\n")

anf-checks =
```

block:
	print('{"student1":[')

	print('{"test1":[')
	daily-max-for-month([list: 20140901, 101.1, 100, 120, 20140902, 500, 450, 100, 
20140904, 90, 300, 299.9, 20140905, 400, 20141001, 20, 20141002, 400, 
500, 300, 20141101, 30], 10)
	print('"end"]},')
	print('{"test2":[')
	daily-max-for-month([list: 20151004, 200, 150, 175], 10)
	print('"end"]}')

	print(']}')
	print("jsonENDShere")
  	nothing
end
```

final-string = as-string + "\n\n" + anf-checks

F.output-file("earthquake-transformed.arr", false).display(final-string)