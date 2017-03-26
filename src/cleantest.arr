import ast as A
import parse-pyret as SP
import file as F
import filelib as FL
import cmdline as C


# -------------- remove tests ----------------

modify-functions = A.default-map-visitor.{
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

  # Remove check blocks
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

file-to-write = string-replace(stud-sub, "student-codes", "cleaning")
tpathstr = string-split(file-to-write, "/final-submission/")
studtrans-dir = tpathstr.first
studsubflname = tpathstr.last()

stud-dir = string-split(studtrans-dir, "cleaning/").last()

checks = base + "/tests/tester.arr"

transform-dir = base + "/cleaning"
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

toparse = F.input-file(stud-sub).read-file()

p = SP.surface-parse(toparse, "test-file.arr")

modified = p.visit(modify-functions)
as-string = modified.tosource().pretty(80).join-str("\n")

final-string = as-string + "\n\n" + F.input-file(checks).read-file()

F.output-file(file-to-write, false).display(final-string)
