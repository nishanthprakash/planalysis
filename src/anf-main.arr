import ast as A
import parse-pyret as SP
import file as F
import filelib as FL
import cmdline as C

base = (C.args).first

anf-checks-file = base + "/tests/anf-checks.arr"
stud-data-dir = base + "/student-codes"

anf-checks = F.input-file(anf-checks-file).read-file()

stud-repos = FL.list-files(stud-data-dir)
two-submissions = [list: "earthquake-1.arr", "earthquake-2.arr"]

var function-counter = 0

# -------------- ANF ----------------

modify-functions-anf = A.default-map-visitor.{
  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, where-block, blocky):
    A.s-fun(
      l,
      name,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      "",
      body.visit(self),
      none,
      none,
      blocky)
  end,

  method s-app(self, l, _fun, args):
    block:
      function-counter := function-counter + 1

      f-id = num-to-string(function-counter)
      fs-len = string-length(f-id)

      # Have to remove and put back each i/p argument separately as the arglist is not a list in the program runtime 
      # (the list cant be directly captured, thought it could probably be splat into i/p list of the funtion)

      let-args = args.foldr(
        lam(x, y):
          cases(List) y:
            | empty => link(A.s-let(l, A.s-bind(l, false, A.s-name(l,  "_" + f-id + "__in_1"), A.a-blank), x.visit(self), false), y)
            | link(first, rest) => 
              in-index = string-to-number(string-substring(first.name.id.s, 6 + fs-len, string-length(first.name.id.s))).value + 1
              arg-name = "_" + f-id + "__in_" + num-to-string(in-index)
              link(A.s-let(l, A.s-bind(l, false, A.s-name(l, arg-name), A.a-blank), x.visit(self), false), y)  
          end
        end, empty)

      list-argids = args.foldr(
        lam(_, y):
          cases(List) y:
            | empty => link(A.s-id(l, A.s-name(l, "_" + f-id + "__in_1")), y)
            | link(first, rest) => 
              in-index = string-to-number(string-substring(first.id.s, 6 + fs-len, string-length(first.id.s))).value + 1
              arg-name = "_" + f-id + "__in_" + num-to-string(in-index)
              link(A.s-id(l, A.s-name(l, arg-name)), y)  
          end
        end, empty)

      function-name = 
	      cases(A.Expr) _fun:
	      	| s-id(loc, f-name) => 	
	      		cases(A.Name) f-name:
	      			| s-name(loctn, f-str) => f-str
	      			| else => "__"
	      		end
	      	| s-dot(ll, aa, bb) => 
	            cases(A.Expr) aa:
	            	| s-id(aloc, af-name) =>  
	              		cases(A.Name) af-name:
	                		| s-name(aloctn, af-str) => af-str + "." + bb
	                		| else => "____"
	                	end
	            	| else => "___"
	            end
	      	| else => "lambda"
	      end

      in-and-out-rev = (link( 
          A.s-let(l, 
            A.s-bind(l, false, A.s-name(l, "_" + f-id + "__out"), A.a-blank), 
            A.s-app(l, 
              _fun.visit(self), 
              list-argids), false), 
          let-args.reverse()))

      addto-acc = A.s-assign(l, A.s-name(l, "dxaxt"), 
                    A.s-app(l, A.s-dot(l, A.s-id(l, A.s-name(l, "dxaxt")), "append"), 
                      [list: A.s-construct(l, A.s-construct-normal, A.s-id(l, A.s-name(l, "list")), 
                          [list: A.s-tuple(l, 
                              [list:
                                A.s-id(l, A.s-name(l, f-id)), 
                                A.s-str(l, function-name), 
                                A.s-id(l, A.s-name(l, "_" + f-id + "__out"))] + list-argids )])]))

      block-out = A.s-id(l, A.s-name(l, "_" + f-id + "__out"))

      anf-complete = link(block-out, link(addto-acc, in-and-out-rev)).reverse()

      A.s-user-block(l, A.s-block(l, anf-complete))
    end
  end,

  method s-check(self, l, name, body, keyword-check):
    nothing
  end
}

# ------------------------------------------

transform-dir = base + "/transformed"

when not(FL.exists(transform-dir)):
  FL.create-dir(transform-dir)
end

for each(stud-dir from stud-repos):
  block:
    student-file-pre = stud-data-dir + "/" + stud-dir + "/final-submission"

    studtrans-dir = transform-dir + "/" + stud-dir
    fs-out-dir =  studtrans-dir + "/final-submission"
    when not(FL.exists(studtrans-dir)):
      FL.create-dir(studtrans-dir)
    end
    when not(FL.exists(fs-out-dir)):
        FL.create-dir(fs-out-dir)
    end

    for each(stud-sub from two-submissions):
      
      block:
        function-counter := 0
        student-file = student-file-pre + "/" + stud-sub
        student-file-out-pre = fs-out-dir + "/" + stud-sub
        student-file-out-anf = string-replace(student-file-out-pre, ".arr", "-anf.arr")

        p = SP.surface-parse(F.input-file(student-file).read-file(), "test-file.arr")

        modified-anf = p.visit(modify-functions-anf)
        as-string-anf = modified-anf.tosource().pretty(80).join-str("\n")

        final-string-anf = 
```
import file as xFx

import filelib as xFLx

var dxaxt = empty

``` 
  + string-replace(as-string-anf, "provide *", "") +  "\n\n" +
```

block:
``` 
  + anf-checks + 
```

when not(xFLx.exists("``` + base + "/anfdata" + ```")):
  xFLx.create-dir("``` + base + "/anfdata" + ```") 
end

```
+ '\n\nxFx.output-file("' + base + "/anfdata/" + stud-dir + "-" + stud-sub + "-anfdata.arr" + '", false).display("provide * \\n\\nD = " + torepr({' + stud-dir + '; "' + stud-sub + '"; collect-dxaxt}))\n\nnothing\n\nend'
        

        F.output-file(student-file-out-anf, false).display(final-string-anf)
      end

    end
  end
end