import ast as A
import parse-pyret as SP
import file as F
import filelib as FL
import file('../data/tests/anf-checks.arr') as anf-checks
import cmdline as C


var function-counter = 0
var datadefs = ""
funcs = [list-set: "reverse", "sort-by", "sort", "filter", "partition", "find", "split-at", "map", "each", "fold", "foldl", "foldr"]

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
      var isdotobj = false
      var dotobj = none
      var let-args = empty
      function-counter := function-counter + 1

      f-id = num-to-string(function-counter)
      fs-len = string-length(f-id)

      function-name = 
        cases(A.Expr) _fun:
          | s-id(loc, f-name) =>  
            cases(A.Name) f-name:
              | s-name(loctn, f-str) => f-str
              | else => "___"
            end
          | s-dot(ll, aa, bb) => 
            block:
              isdotobj := true
              dotobj := aa
              bb
            end
          | else => "lambda"
        end

      # Have to remove and put back each i/p argument separately as the arglist is not a list in the program runtime 
      # (the list cant be directly captured, thought it could probably be splat into i/p list of the funtion)
      # Besides, more importantly, the args need to be eval'd only once (incase they have side effects)
      # got to collect the output too, there is no way to torepr the in-out anf tuple before execution

      let-args := args.foldr(
        lam(x, y):
          cases(List) y:
            | empty => link(A.s-let(l, A.s-bind(l, false, A.s-name(l,  "_" + f-id + "__in_1"), A.a-blank), x.visit(self), false), y)
            | link(first, rest) => 
              in-index = string-to-number(string-substring(first.name.id.s, 6 + fs-len, string-length(first.name.id.s))).value + 1
              arg-name = "_" + f-id + "__in_" + num-to-string(in-index)
              link(A.s-let(l, A.s-bind(l, false, A.s-name(l, arg-name), A.a-blank), x.visit(self), false), y)  
          end
        end, empty)

      when isdotobj:
        let-args := link(A.s-let(l, A.s-bind(l, false, A.s-name(l,  "_" + f-id + "__obj"), A.a-blank), dotobj.visit(self), false), let-args)
      end 

      # Reverse orders of arg evals, check if it matters

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

      in-and-out-rev =
        if not(isdotobj):  
          A.s-let(l, 
              A.s-bind(l, false, A.s-name(l, "_" + f-id + "__out"), A.a-blank), 
              A.s-app(l, 
                _fun.visit(self), 
                list-argids), false)
        else:
          if function-name == "sort-by":
            A.s-let(l, 
              A.s-bind(l, false, A.s-name(l, "_" + f-id + "__out"), A.a-blank), 
              A.s-app(l, 
                A.s-id(l, A.s-name(l, function-name)), 
                [list: A.s-id(l, A.s-name(l, "_" + f-id + "__obj"))] + list-argids), false)
          else if funcs.member(function-name):
            A.s-let(l, 
              A.s-bind(l, false, A.s-name(l, "_" + f-id + "__out"), A.a-blank), 
              A.s-app(l, 
                A.s-id(l, A.s-name(l, function-name)), 
                list-argids + [list: A.s-id(l, A.s-name(l, "_" + f-id + "__obj"))]), false)
          else:
            A.s-let(l, 
              A.s-bind(l, false, A.s-name(l, "_" + f-id + "__out"), A.a-blank), 
              A.s-app(l, 
                A.s-dot(l, A.s-id(l, A.s-name(l, "_" + f-id + "__obj")), function-name), 
                list-argids), false)
          end
        end
          

      inc-count = A.s-assign(l, A.s-name(l, "xoxcx"), A.s-op(l, l, "op+", A.s-id(l, A.s-name(l, "xoxcx")), A.s-num(l, 1)))
      start-capture = A.s-let(l, A.s-bind(l, true, A.s-name(l, "sxtxr"), A.a-blank), A.s-id(l, A.s-name(l, "xoxcx")), false)
      stop-capture = A.s-let(l, A.s-bind(l, true, A.s-name(l, "sxtxp"), A.a-blank), A.s-id(l, A.s-name(l, "xoxcx")), false)

      addto-acc = A.s-assign(l, A.s-name(l, "dxaxt"), 
                    A.s-app(l, A.s-dot(l, A.s-id(l, A.s-name(l, "dxaxt")), "append"), 
                      [list: A.s-construct(l, A.s-construct-normal, A.s-id(l, A.s-name(l, "list")), 
                          [list: A.s-tuple(l, 
                              [list:
                                A.s-id(l, A.s-name(l, "sxtxr")),
                                A.s-id(l, A.s-name(l, "sxtxp")),
                                A.s-id(l, A.s-name(l, f-id)), 
                                A.s-str(l, function-name),
                                A.s-construct(l, A.s-construct-normal, A.s-id(l, A.s-name(l, "list")), list-argids),
                                A.s-id(l, A.s-name(l, "_" + f-id + "__out")),
                                if isdotobj:
                                  A.s-id(l, A.s-name(l, "_" + f-id + "__obj"))
                                else:
                                  A.s-id(l, A.s-name(l, "none"))
                                end])])]))

      block-out = A.s-id(l, A.s-name(l, "_" + f-id + "__out"))

      anf-complete = let-args + [list: inc-count] + [list: start-capture] + [list: in-and-out-rev] + [list: inc-count] + [list: stop-capture] + [list: addto-acc] + [list: block-out]

      A.s-user-block(l, A.s-block(l, anf-complete))
    end
  end,

  # Need to take care of name conflicts
  method s-data(
      self,
      l :: A.Loc,
      name :: String,
      params :: List<A.Name>, # type params
      mixins :: List<A.Expr>,
      variants :: List<A.Variant>,
      shared-members :: List<A.Member>,
      _check-loc :: Option<A.Loc>,
      _check :: Option<A.Expr>
    ):

    # Just converting the data definitions into strings and collecting them  
    # as we dont need to wait for this to execute as opposed to ANF tuples
    # eval is done once in student program env and this prog env.
    block:
		sdata =
		A.s-data(
	        l,
	        name,
	        params.map(_.visit(self)),
	        mixins.map(_.visit(self)),
	        variants.map(_.visit(self)),
	        shared-members.map(_.visit(self)),
	        _check-loc,
	        self.option(_check)
		)

	    datadefs := datadefs + "\\n\\n" + sdata.tosource().pretty(80).join-str("\\n")
	    sdata
    end
  end,

  # Remove check statements
  method s-program(self, loc, _provide, provided-types, imports, body):
    st = filter(lam(x): 
        cases(A.Expr) x:
          | s-check(l, n, b, k) => false
          | else => true
        end
        end, body.stmts)
    
    A.s-program(loc, _provide.visit(self), provided-types.visit(self), imports.map(_.visit(self)), (A.s-block(loc, st)).visit(self))
  end,
}

# ------------------------------------------

stud-sub = (C.args).first

bpathstr = string-split(stud-sub, "/student-codes/")
base = bpathstr.first

file-to-write = string-replace(stud-sub, "student-codes", "transformed")
tpathstr = string-split(file-to-write, "/final-submission/")
studtrans-dir = tpathstr.first
studsubflname = tpathstr.last()

stud-dir = string-split(studtrans-dir, "transformed/").last()

lists-file = base + "/code-stubs/lists.arr"

transform-dir = base + "/transformed"
when not(FL.exists(transform-dir)):
  FL.create-dir(transform-dir)
end
fs-out-dir =  studtrans-dir + "/final-submission"
when not(FL.exists(studtrans-dir)):
  FL.create-dir(studtrans-dir)
end
when not(FL.exists(fs-out-dir)):
    FL.create-dir(fs-out-dir)
end

var blockstr = 0
var tind = 0

block:
	datadefs := ""
  function-counter := 0

  student-file-out-anf = string-replace(file-to-write, ".arr", "-anf.arr")

  plst = F.input-file(lists-file).read-file()

  toparse = string-replace("provide *" + "\n\n" + string-replace(plst, "#INSERTIMPORTS", "import file as xFx\n\nimport filelib as xFLx\n\nvar dxaxt = empty\n\nvar xoxcx = 0") + "\n\n" + F.input-file(stud-sub).read-file(), "provide *", "")
  p = SP.surface-parse(toparse, "test-file.arr")

  modified-anf = p.visit(modify-functions-anf)
  as-string-anf = modified-anf.tosource().pretty(80).join-str("\n")

  tind := 0
  blockstr := ""
  for each(test from anf-checks.ins) block:
    tind := tind + 1

    blockstr := blockstr + 
```
dxaxt := empty
xoxcx := 0
``` 
+ "\n\n" + test + "\n\n" +

'xFx.output-file("' + base + "/anfdata/" + stud-dir + "-" + string-substring(studsubflname, 11, 12) + "_" + num-to-string(tind) + ".arr" + '", false).display("provide * \\n\\n' + datadefs + '\\n\\ndat = " + string-replace(torepr({' + stud-dir + '; ' + string-replace(string-replace(studsubflname, ".arr", ""), "earthquake-", "") + '; dxaxt}), "<function>", "\\\"<function>\\\""))' + "\n\n"

  end

  # appending data to prevent order dependency
  final-string-anf = as-string-anf +  "\n\n" +

```
block:
``` 
+ "\n\n" + 
```
when not(xFLx.exists("``` + base + "/anfdata" + ```")):
xFLx.create-dir("``` + base + "/anfdata" + ```") 
end
```  
+ "\n\n" + blockstr

+ '\n\nnothing\n\nend'

  F.output-file(student-file-out-anf, false).display(final-string-anf)

end
