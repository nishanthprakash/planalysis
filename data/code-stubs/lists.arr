import global as _
import option as O
import either as E
import equality as equality
import valueskeleton as VS

#INSERTIMPORTS

shadow none = O.none
shadow is-none = O.is-none
shadow some = O.some
shadow is-some = O.is-some

left = E.left
right = E.right
type Either = E.Either

shadow each = block:
  fun _each<a>(f_each :: (a -> Nothing), lst :: List<a>) -> Nothing block:
    fun help_each(l1):
      if is-empty(l1) block:
        nothing
      else:
        f_each(l1.first)
        help_each(l1.rest)
      end
    end
    help_each(lst)
    nothing
  end
  _each
end

shadow reverse-help = block:
  fun _reverse-help<a>(lst :: List<a>, tail :: List<a>) -> List<a>:
    if is-empty(lst) block:
      tail
    else:
      _reverse-help(lst.rest ,link(lst.first, tail))
    end
  end
  _reverse-help
end

shadow reverse = block:
  fun _reverse<a>(lst :: List<a>) -> List<a>: reverse-help(lst, empty) end
  _reverse
end

shadow sort-by = block:
  fun _sort-by<a>(lst :: List<a>, cmp :: (a, a -> Boolean), eq :: (a, a -> Boolean)) -> List<a> block:
    if is-empty(lst) block:
      empty
    else:
      pivot = lst.first
      var are-lt = empty
      var are-eq = empty
      var are-gt = empty
      each(lam(e):
          if cmp(e, pivot):     are-lt := link(e, are-lt)
          else if eq(e, pivot): are-eq := link(e, are-eq)
          else:                 are-gt := link(e, are-gt)
          end
        end, lst)
      less =    _sort-by(are-lt, cmp, eq)
      equal =   are-eq
      greater = _sort-by(are-gt, cmp, eq)
      less.append(equal.append(greater))
    end
  end
  _sort-by
end

shadow sort = block:
  fun _sort<a>(lst :: List<a>) -> List<a>:
    sort-by(lst, lam(e1,e2): e1 < e2 end, within(~0))
  end
  _sort
end

shadow filter = block:
  fun _filter<a>(f_filter :: (a -> Boolean), lst :: List<a>) -> List<a>:
    if is-empty(lst):
      empty
    else:
      if f_filter(lst.first): link(lst.first, _filter(f_filter, lst.rest))
      else:            _filter(f_filter, lst.rest)
      end
    end
  end
  _filter
end

shadow partition = block:
  fun _partition<a>(f_partition :: (a -> Boolean), lst :: List<a>) -> {is-true :: List<a>, is-false :: List<a>} block:
    var is-true = empty
    var is-false = empty
    fun help_partition(inner-lst):
      if is-empty(inner-lst) block:
        nothing
      else:
        help_partition(inner-lst.rest)
        if f_partition(inner-lst.first):
          is-true := link(inner-lst.first, is-true)
        else:
          is-false := link(inner-lst.first, is-false)
        end
      end
    end
    help_partition(lst)
    { is-true: is-true, is-false: is-false }
  end
  _partition
end

shadow find = block:
  fun _find<a>(f_find :: (a -> Boolean), lst :: List<a>) -> O.Option<a>:
    if is-empty(lst):
      none
    else:
      if f_find(lst.first):
        some(lst.first)
      else:
        _find(f_find, lst.rest)
      end
    end
  end
  _find
end

shadow split-at = block:
  fun _split-at<a>(n :: Number, lst :: List<a>) -> { prefix :: List<a>, suffix :: List<a> } block:
    when n < 0:
      raise("Invalid index")
    end
    var prefix = empty
    var suffix = empty
    fun help_split-at(ind, l):
      if ind == 0: suffix := l
      else:
        cases(List) l block:
          | empty => raise("Index too large")
          | link(fst, rst) =>
            help_split-at(ind - 1, rst)
            prefix := link(fst, prefix)
        end
      end
    end
    help_split-at(n, lst)
    { prefix: prefix, suffix: suffix }
  end
  _split-at
end

shadow map = block:
  fun _map<a, b>(f_map :: (a -> b), lst :: List<a>) -> List<b> block:
    if is-empty(lst):
      empty
    else:
      link(f_map(lst.first), _map(f_map, lst.rest))
    end
  end
  _map
end


shadow fold = block:
  fun _fold<a, b>(f_fold :: (a, b -> a), acc :: a, lst :: List<b>) -> a:
    if is-empty(lst):
      acc
    else:
      _fold(f_fold, f_fold(acc, lst.first), lst.rest)
    end
  end
  _fold
end


rec shadow foldl = fold

shadow foldr = block:
  fun _foldr<a, b>(f_foldr :: (a, b -> a), base :: a, lst :: List<b>) -> a:
    if is-empty(lst):
      base
    else:
      f_foldr(_foldr(f_foldr, base, lst.rest), lst.first)
    end
  end
  _foldr
end