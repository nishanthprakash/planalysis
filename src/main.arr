import ast as A
import parse-pyret as SP
import file as F
import filelib as FL
import cmdline as C

base = (C.args).first

where-checks-file = base + "/tests/where-checks.arr"
anf-checks-file = base + "/tests/anf-checks.arr"
stud-data-dir = base + "/student-codes"

where-checks = F.input-file(where-checks-file).read-file()
anf-checks = F.input-file(anf-checks-file).read-file()

stud-repos = FL.list-files(stud-data-dir)
two-submissions = [list: "earthquake-1.arr", "earthquake-2.arr"]

var function-counter = 0

# -------------- where ----------------

fun replace-where(wblock, fname):
  var whrblock = none
  whast = SP.surface-parse(string-replace(where-checks,"fxtxexsxt", fname), "name-of-program.arr")
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


modify-functions-where = A.default-map-visitor.{
  method s-fun(self, l, name, params, args, ann, doc, body, where-block, blocky):
    A.s-fun(
      l,
      name,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      "",
      body.visit(self),
      replace-where(where-block, name),
      blocky)
  end,

  method s-check(self, l , name, body, keyword-check):
    nothing
  end
}

# -------------- ANF ----------------



modify-functions-anf = A.default-map-visitor.{
  method s-fun(self, l, name, params, args, ann, doc, body, where-block, blocky):
    A.s-fun(
      l,
      name,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      "",
      body.visit(self),
      none,
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

      add-prints-rev =  link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-str(l, '"}},')]),
        link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-id(l, A.s-name(l, "_" + f-str + "__out"))]), 
          link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-str(l, '", "ouput":"')]), 
            link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-construct(l, A.s-construct-normal, A.s-id(l, A.s-name(l, "list")), list-argids)]), 
              link(A.s-app(l, A.s-id(l, A.s-name(l, "print")), [list: A.s-str(l, '{"' + f-str + '":{"input":"')]), 
                in-and-out-rev)))))

      block-out = A.s-id(l, A.s-name(l, "_" + f-str + "__out"))

      anf-complete = (link(block-out, add-prints-rev).reverse())

      A.s-user-block(l, A.s-block(l, anf-complete))
    end
  end,

  method s-check(self, l, name, body, keyword-check):
    nothing
  end
}


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
        student-file-out-where = string-replace(student-file-out-pre, ".arr", "-where.arr")

        p = SP.surface-parse(F.input-file(student-file).read-file(), "test-file.arr")

        modified-where = p.visit(modify-functions-where)
        as-string-where = modified-where.tosource().pretty(80).join-str("\n")
        F.output-file(student-file-out-where, false).display(as-string-where)

        modified-anf = p.visit(modify-functions-anf)
        as-string-anf = modified-anf.tosource().pretty(80).join-str("\n")
        final-string-anf = as-string-anf + "\n\n" + anf-checks
        F.output-file(student-file-out-anf, false).display(final-string-anf)
      end

    end
  end
end