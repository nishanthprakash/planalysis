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

  method s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    #s-app(l, _fun.visit(self), args.map(_.visit(self)))

    let-args = args.foldr(
      lam(x, y):
        cases List y:
	  | empty => link(s-let(l, s-bind(l, false, "_f__in_" + num-to-string(1), a-blank), x.visit(self), false), y)
          | link (first, rest) => 
            arg-name = "_f__in_" + number-to-string(string-to-number(string-substring(first.b.id, 7, string-length(first.b.id))) + 1)
            link(s-let(l, s-bind(l, false, arg-name, a-blank), x.visit(self), false), y)  
        end
      end, empty)

    list-argids = args.foldr(
      lam(_, y):
        cases List y:
	  | empty => link(s-id(l, "_f__in_"), y)
          | link (first, rest) =>
            arg-name = "_f__in_" + number-to-string(string-to-number(string-substring(first.b.id, 7, string-length(first.b.id))) + 1)
            link(s-let(l, arg-name), y)  
        end
      end, empty)

    
    in-and-out = (link( 
      s-let(l, 
        s-bind(l, false, "_f__out", a-blank), 
        s-app(l, 
          _fun.visit(self), 
          list-argids), false), 
      let-args.reverse())).reverse()

    print-temp = in-and-out.foldl(
      lam(x, y):
        cases List y:
	  | empty => s-op(l, l, op+, s-str(l, '{"f":['), s-app(l, s-id(l, num-to-string), [list: s-id(l, "_f__in_1")]))
          | link (first, rest) =>
            arg-name = x.b.id
            s-op(l, l, op+, s-op(l, l, op+, y, s-str(l, ', ')), s-app(l, s-id(l, num-to-string), [list: s-id(l, arg-name)]))
        end
      end, empty)

    printargs-anf = s-op(l, l, op+, print-temp, s-str(l, ']}')

    print-app = s-app(l, s-id(l, print), link(printargs-anf, empty))

    block-out = s-id(l, "_f__out")

    anf-complete = (link(block-out, link(print-app, in-and-out.reverse())).reverse()

    s-user-block(l, s-block(l, anf-complete))
  end
}

modified = p.visit(modify-functions)

as-string = modified.tosource().pretty(80).join-str("\n")

F.output-file("earthquake-transformed.arr", false).display(as-string)

