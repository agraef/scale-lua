#!/usr/bin/env lua

prog_version = "0.4"

function prog_path()
   local s = debug.getinfo(2, "S").source:sub(2)
   if not s then
      -- direct invocation
      return ""
   end
   -- if s is a link, follow it to its destination
   local f = io.popen('readlink ' .. string.format('%q', s))
   if f then
      local t = f:read()
      if t then
	 s = t
      end
      f:close()
   end
   local path = s:match("(.*/)")
   return path and path or ""
end

prefix = prog_path()
--print("prefix = ", prefix)

-- Tcl/Gnocl GUI
tk = require("tk")
if prefix ~= "" then
   tk.set("prefix", prefix)
end

-- need this to print table values (available from LuaRocks)
inspect = require("inspect")

-- load the Octave interface
oct = require("octave")

-- load scale.m into Octave
oct.eval("source " .. prefix .. "lib/scale.m", 0)

-- infinity (some scale data uses this for dmax, not sure why)
inf = 1/0

--[[ Load the GUI and the rendering pipeline which is defined in the
   accompanying Tcl script, scale.tcl. ]]

function readfile(file)
   local f = assert(io.open(file, "rb"))
   local content = f:read("*all")
   f:close()
   return content
end

scaleprog = readfile(prefix .. "lib/scale.tcl")

-- list helper functions

-- concatenate tables
function tableconcat(t1, t2)
   local res = {}
   for i=1,#t1 do
      table.insert(res, t1[i])
   end
   for i=1,#t2 do
      table.insert(res, t2[i])
   end
   return res
end

-- reverse a table
function reverse(list)
   local res = {}
   for _, v in ipairs(list) do
      table.insert(res, 1, v)
   end
   return res
end

-- arithmetic sequences
function seq(from, to, step)
   step = step or 1;
   local sgn = step>=0 and 1 or -1
   local res = {}
   while sgn*(to-from) >= 0 do
      table.insert(res, from)
      from = from + step
   end
   return res
end

-- cycle through a table
function cycle(t, i)
   local n = #t
   if n > 0 then
      while i > n do
	 i = i - n
      end
   end
   return t[i]
end

-- some functional programming goodies

function all(list, fn)
   for _, v in ipairs(list) do
      if not fn(v) then
	 return false
      end
   end
   return true
end

function any(list, fn)
   for _, v in ipairs(list) do
      if fn(v) then
	 return true
      end
   end
   return false
end

function map(list, fn)
   local res = {}
   for _, v in ipairs(list) do
      table.insert(res, fn(v))
   end
   return res
end

function reduce(list, acc, fn)
   for _, v in ipairs(list) do
      acc = fn(acc, v)
   end
   return acc
end

function collect(list, acc, fn)
   local res = {acc}
   for _, v in ipairs(list) do
      acc = fn(acc, v)
      table.insert(res, acc)
   end
   return res
end

function sum(list)
   return reduce(list, 0, function(a,b) return a+b end)
end

function prd(list)
   return reduce(list, 1, function(a,b) return a*b end)
end

function sums(list)
   return collect(list, 0, function(a,b) return a+b end)
end

function prds(list)
   return collect(list, 1, function(a,b) return a*b end)
end

--helper functions to create bitsets

function bitset(list)
   return reduce(list, 0, function(a,b) return a|bit(b) end)
end

function bit(n)
   return 1 << n
end

-- Determine the prime factors of an integer. The result is a list with the
-- prime factors in non-decreasing order.

function factor(n)
   local factors = {}
   if n<0 then n = -n end
   while n % 2 == 0 do
      table.insert(factors, 2)
      n = math.floor(n / 2)
   end
   local p = 3
   while p <= math.sqrt(n) do
      while n % p == 0 do
	 table.insert(factors, p)
	 n = math.floor(n / p)
      end
      p = p + 2
   end
   if n > 1 then -- n must be prime
      table.insert(factors, n)
   end
   return factors
end

-- Collect the factors of the integer n and return them as a list of pairs
-- {p,k} where p are the prime factors in ascending order and k the
-- corresponding (nonzero) multiplicities. If the given number is a pair {p,
-- q}, considers p/q as a rational number and returns its prime factors with
-- positive or negative multiplicities.

function factors(x)
   if type(x) == "table" then
      local n, m = table.unpack(x)
      local pfs, nfs, mfs = {}, factors(n), factors(m)
      -- merge the factors in nfs and mfs into a single list
      local i, j, k, N, M = 1, 1, 1, #nfs, #mfs
      while i<=N or j<=M do
	 if j>M or (i<=N and mfs[j][1]>nfs[i][1]) then
	    pfs[k] = nfs[i]
	    k = k+1; i = i+1
	 elseif i>N or (j<=M and nfs[i][1]>mfs[j][1]) then
	    pfs[k] = mfs[j]
	    pfs[k][2] = -mfs[j][2]
	    k = k+1; j = j+1
	 else
	    pfs[k] = nfs[i]
	    pfs[k][2] = nfs[i][2] - mfs[j][2]
	    k = k+1; i = i+1; j = j+1
	 end
      end
      return pfs
   else
      local pfs, pf = {}, factor(x)
      if next(pf) then
	 local j, n = 1, #pf
	 pfs[j] = {pf[1], 1}
	 for i = 2, n do
	    if pf[i] == pfs[j][1] then
	       pfs[j][2] = pfs[j][2] + 1
	    else
	       j = j+1
	       pfs[j] = {pf[i], 1}
	    end
	 end
      end
      return pfs
   end
end

-- Barlow harmonicities from the Ratio book. These are mostly ripped out of an
-- earlier version of the Raptor random arpeggiator programs (first written in
-- Q, then rewritten in Pure, and finally ported to Lua).

-- Some "standard" 12 tone scales and prime valuation functions for testing
-- purposes.

just = -- standard just intonation, a.k.a. the Ptolemaic (or Didymic) scale
   {  {1,1}, {16,15}, {9,8}, {6,5}, {5,4}, {4,3}, {45,32},
      {3,2}, {8,5}, {5,3}, {16,9}, {15,8}, {2,1}  }
pyth = -- pythagorean (3-limit) scale
   {  {1,1}, {2187,2048}, {9,8}, {32,27}, {81,64}, {4,3}, {729,512},
      {3,2}, {6561,4096}, {27,16}, {16,9}, {243,128}, {2,1}  }
mean4 = -- 1/4 comma meantone scale, Barlow (re-)rationalization
   {  {1,1}, {25,24}, {10,9}, {6,5}, {5,4}, {4,3}, {25,18},
      {3,2}, {25,16}, {5,3}, {16,9}, {15,8}, {2,1}  }

function barlow(p)	return 2*(p-1)*(p-1)/p end
function euler(p)	return p-1 end
-- "mod 2" versions (octave is eliminated)
function barlow2(p)	if p==2 then return 0 else return barlow(p) end end
function euler2(p)	if p==2 then return 0 else return euler(p) end end
function logbarlow(p)	return math.log(barlow(p), 10) end
function logeuler(p)	return math.log(euler(p), 10) end

-- Harmonicity computation.

-- hrm({p,q}, pv) computes the disharmonicity of the interval p/q using the
-- prime valuation function pv.

-- hrm_dist({p1,q1}, {p2,q2}, pv) computes the harmonic distance between two
-- pitches, i.e., the disharmonicity of the interval between {p1,q1} and
-- {p2,q2}.

-- hrm_scale(S, pv) computes the disharmonicity metric of a scale S, i.e., the
-- pairwise disharmonicities of all intervals in the scale. The input is a
-- list of intervals as {p,q} pairs, the output is the distance matrix.

function hrm(x, pv)
   return sum(map(factors(x),
	function(f) local p, k = table.unpack(f)
	   return math.abs(k) * pv(p)
	end))
end

function hrm_dist(x, y, pv)
   local p1, q1 = table.unpack(x)
   local p2, q2 = table.unpack(y)
   return hrm({p1*q2,p2*q1}, pv)
end

function hrm_scale(S, pv)
   return map(S,
	function(s)
	   return map(S, function(t) return hrm_dist(s, t, pv) end)
	end)
end

-- note names, 11-limit nomenclature (originally pilfered from Clarence
-- Barlow's nomen.pas)

nomen = 
  { [-2] = bitset {},             [2] = bitset {},
    [-3] = bitset {3},            [3] = bitset {6},
    [-5] = bitset {0,1,3,4},      [5] = bitset {1,2,5,6},
    [-7] = bitset {2,6},          [7] = bitset {0,3},
   [-11] = bitset {0,1,2,3,4,5}, [11] = bitset {0,1,2,4,5,6}}

steps = {[2]=7, [3]=4, [5]=2, [7]=-6, [11]=3}

-- Internally we encode note names as pairs (n,a) where n is the note number
-- (0=C, 1=D, etc.) and a the number of accidentals (<0: flats, >0: sharps).

base = {["Cb"] = {0,-1}, ["C"] = {0,0}, ["C#"] = {0,1},
	["Db"] = {1,-1}, ["D"] = {1,0}, ["D#"] = {1,1},
	["Eb"] = {2,-1}, ["E"] = {2,0}, ["E#"] = {2,1},
	["Fb"] = {3,-1}, ["F"] = {3,0}, ["F#"] = {3,1},
	["Gb"] = {4,-1}, ["G"] = {4,0}, ["G#"] = {4,1},
	["Ab"] = {5,-1}, ["A"] = {5,0}, ["A#"] = {5,1},
	["Bb"] = {6,-1}, ["B"] = {6,0}, ["B#"] = {6,1}}

-- Shift note name and accidental for given prime factor.

function leap(note, f)
   local function normalize_note(n)
      n = n % 7
      return n<0 and n+7 or n
   end
   function leap_note(n, f)
      local p,k = table.unpack(f)
      return normalize_note(n+k*math.abs(steps[p]))
   end
   function leap_acc(note, f)
      local n,a = table.unpack(note)
      local p,k = table.unpack(f)
      assert(math.abs(k) == 1)
      if nomen[k*p] & bit(n) ~= 0 then
	 local sgn = steps[p] < 0 and -1 or steps[p] > 0 and 1 or 0
	 return a+k*sgn
      else
	 return a
      end
   end
   local n,a = table.unpack(note)
   local p,k = table.unpack(f)
   if k > 1 then
      return leap(leap(note, {p,1}), {p,k-1})
   elseif k < -1 then
      return leap(leap(note, {p,-1}), {p,k+1})
   elseif k ~= 0 then
      return {leap_note(n, f), leap_acc(note, f)}
   else
      return note
   end
end

notes = {"C","D","E","F","G","A","B"}

function note_name(b, x)
   local fs = factors(x)
   -- make sure that f is 11-limit
   for _, v in ipairs(fs) do
      local p,k = table.unpack(v)
      if p > 11 then
	 return nil
      end
   end
   local res = reduce(fs, base[b], leap)
   local n,a = table.unpack(res)
   local k, c
   if a < 0 then
      k, c = -a, "b"
   else
      k, c = a, "#"
   end
   local ret = notes[n+1]
   for i = 1, k do
      ret = ret .. c
   end
   return ret
end

-- Scale graph and rationalization algorithm. This is a port of the Pure
-- implementation.

T = {{{1,1}}, {},
   reverse {{9,8},{10,9},{8,7}},
   reverse {{6,5},{32,27},{8,7}},
   reverse {{5,4},{81,64},{32,25}}}

function scale_graph(T, dmax, pv)
   -- node set
   local V = reduce(T, {}, tableconcat)
   local m = #T
   local n = #V
   -- node indices
   local k = {}
   for i = 1, m do
      for j, _ in pairs(T[i]) do
	 table.insert(k, i)
      end
   end
   -- disharmonicity metric
   local M = hrm_scale(V, pv)
   -- edges (adjacency lists)
   local E = {}
   for i = 1, n do
      E[i] = {}
      for j = 1, n do
	 if k[i] ~= k[j] and M[i][j] <= dmax then
	    table.insert(E[i], j)
	 end
      end
   end
   return V, E, M, k
end

-- Scale rationalization algorithm.

function rationalize(T, heur, dmax, pv)
   local V, E, M, k = scale_graph(T, dmax, pv)
   -- calculate the disharmonicities of the candidates
   local M0 = map(V, function (x) return hrm(x, pv) end)
   -- calculate the max possible clique size, which equals the number of
   -- non-empty candidate sets
   local n = reduce(T, 0, function (n, C) return next(C) and n+1 or n end)
   -- actual node list (just the indices)
   local U = seq(1, #V)
   -- Search heuristic. "First" just considers the nodes in the natural
   -- order. "Best" tries to minimize the total harmonic distance.
   function first(C, V)
      return V
   end
   function best(C, V)
      -- Minimize the sum of the harmonic distances.
      function cmp(a, b)
	 -- if the distance totals are equal, compare based on plain weights,
	 -- or on index numbers
	 local x, y = M0[a.v], M0[b.v]
	 return a.d < b.d or a.d == b.d and
	    (x < y or x == y and a.v < b.v)
      end
      function dist(v)
	 local d = reduce(C, 0, function (d, c) return d+M[c][v] end)
	 return { v = v, d = d }
      end
      V = map(V, dist)
      table.sort(V, cmp)
      V = map(V, function (a) return a.v end)
      return V
   end
   local h
   if heur == "First" then
      h = first
   elseif heur == "Best" then
      h = best
   else
      error("Unknown search heuristic")
   end
   -- turn adjacency lists into sets, for quicker adjacency tests
   function makeset(V)
      local S = {}
      for _, v in pairs(V) do
	 S[v] = true
      end
      return S
   end
   -- Carraghan/Pardalos maximum clique algorithm.
   function maxclique(V, E)
      function search(Cmax, C, V)
	 if not tk.ready() or breakflag then
	    -- interrupt, return the largest clique found so far
	    return #C > #Cmax and C or Cmax
	 elseif #Cmax >= n or #C+#V <= #Cmax then
	    -- prune
	    return Cmax
	 elseif #V == 0 then
	    -- new maximum clique
	    log2_cb(#C/n)
	    return C
	 else
	    local U = h(C, V)
	    for _, v in ipairs(U) do
	       local C = {v, table.unpack(C)}
	       local W = {}
	       for _, w in ipairs(V) do
		  if w ~= v and E[v][w] then
		     table.insert(W, w)
		  end
	       end
	       Cmax = search(Cmax, C, W)
	    end
	    return Cmax
	 end
      end
      return reverse(search({}, {}, h({}, V)))
   end
   local C = #V==0 and {} or maxclique(U, map(E, makeset))
   local l = collect(T, 0, function (n, C) return n+#C end)
   local T = map(C, function (i)
		    local j = k[i]
		    return {j, i-l[j], V[i]}
   end)
   return T
end

-- Printable representation of scale items.

function pscale(x)
   if type(x) == "table" then
      local p,q = table.unpack(x)
      if type(p) == "number" and type(q) == "number" then
	 return string.format("%d/%d", p, q)
      else
	 return ""
      end
   elseif type(x) == "number" then
      return tostring(x)
   else
      return ""
   end
end

-- Map ratios to cent values.

function cent(x)
   if type(x) == "table" then
      local p,q = table.unpack(x)
      if type(p) == "number" and type(q) == "number" then
	 return 1200*math.log(p/q)/math.log(2)
      else
	 return nil
      end
   elseif type(x) == "string" then
      local p,q = x:match("^(%d+)/(%d+)$")
      if p and q then
	 p,q = tonumber(p), tonumber(q)
	 return 1200*math.log(p/q)/math.log(2)
      else
	 return nil
      end
   else
      return x
   end
end

-- Map ratios to note names.

function name(x)
   if type(x) == "table" then
      local s = note_name("C", x)
      if s then
	 return s
      else
	 return ""
      end
   elseif type(x) == "string" then
      local p,q = x:match("^(%d+)/(%d+)$")
      if p and q then
	 p,q = tonumber(p), tonumber(q)
	 return name({p,q})
      else
	 return ""
      end
   else
      return ""
   end
end

-- Map ratios to interval descriptions.

function descr(x)
   if type(x) == "table" then
      local p,q = table.unpack(x)
      if type(p) == "number" and type(q) == "number" then
	 local s = string.format("%d/%d", p, q)
	 return descr(s)
      else
	 return ""
      end
   elseif type(x) == "string" then
      local s = intnam[x]
      if s then
	 return s
      else
	 return ""
      end
   else
      return ""
   end
end

-- Map ratios to factors.

function facts(x)
   if type(x) == "table" then
      local fs = factors(x)
      if fs then
	 if #fs == 0 then
	    fs = {{1,1}}
	 end
	 local a = {}
	 local b = {}
	 for _,x in ipairs(fs) do
	    local p,k = table.unpack(x)
	    if k >= 0 then
	       table.insert(a, string.format("%d^%d", p, k))
	    else
	       table.insert(b, string.format("/ %d^%d", p, -k))
	    end
	 end
	 a = table.concat(a, " ") -- numerator
	 b = table.concat(b, " ") -- denominator
	 if #b == 0 then
	    return a
	 elseif #a == 0 then
	    return b
	 else
	    return a .. " " .. b
	 end
      else
	 return ""
      end
   elseif type(x) == "string" then
      local p,q = x:match("^(%d+)/(%d+)$")
      if p and q then
	 p,q = tonumber(p), tonumber(q)
	 return facts({p,q})
      else
	 return ""
      end
   else
      return ""
   end
end

-- Compute node labels.

function node_labels(v, t)
   if v == "0" then
      return {}
   elseif t == "Ordinal" then
      local k = #state.scale
      return map(seq(0, k-1), tostring)
   elseif t == "Pitch" then
      return map(state.scale, pscale)
   elseif t == "Cent" then
      -- we round these to 100ths of a cent for readability
      return map(state.scale, function (x)
		    return tostring(math.floor(cent(x)*100+0.5)/100)
      end)
   elseif t == "Note" then
      return map(state.scale, name)
   elseif t == "Factors" then
      return map(state.scale, facts)
   elseif t == "Interval" then
      return map(state.scale, descr)
   end
end

-- Parse and pretty-print scales.

function parse_scale()
   local scale = tk.split(tk.get("scale"))
   scale = map(scale, scale_val)
   -- Add the base tone if needed.
   while scale[1] == 0.0 or pscale(scale[1]) == "1/1" do
      table.remove(scale, 1)
   end
   table.insert(scale, 1, {1,1})
   tk.set("scale", unparse_scale(scale))
   return scale
end

--[[ We assume the Scala format here. That is, each scale point is either a
   floating point value, indicating cents, or an integer ratio p/q, or a
   single integer x (meaning the ratio x/1). Cent values must be nonnegative,
   integers positive. Integers must be representable as signed 32 bit ints.
   NOTE: We don't enforce that ratios are >1 even though it makes sense, since
   that requirement isn't in the Scala file format spec. ]]

function gcd(a, b)
   return b==0 and a or gcd(b,a%b)
end

bad_scale_data = nil

function scale_val(s)
   bad_scale_data = nil
   local p,q = s:match("^(%d+)/(%d+)$")
   if p and q then
      p,q = tonumber(p), tonumber(q)
      if p>0 and q>0 then
	 local g = gcd(p, q)
	 return {p//g,q//g}
      end
   end
   local x = s:match("^(%d+)$")
   if x then
      x = math.tointeger(s)
      if x and x>0 then
	 return {x,1}
      end
   end
   x = tonumber(s)
   if x and x>=0.0 then
      return x
   end
   bad_scale_data = s
   error("Bad scale data")
end

function unparse_scale(scale, sep)
   if not sep then
      sep = " "
   end
   return table.concat(map(scale, pscale), sep)
end

-- prime valuation function table

pval_funs = {
   ["Barlow"] = barlow, ["Euler"] = euler,
   ["Barlow/2"] = barlow2, ["Euler/2"] = euler2,
   ["Log Barlow"] = logbarlow, ["Log Euler"] = logeuler
}

pval_fun_names = {
   [barlow] = "barlow", [euler] = "euler",
   [barlow2] = "barlow2", [euler2] = "euler2",
   [logbarlow] = "logbarlow", [logeuler] = "logeuler",
}

pval_names = {}
for k,v in pairs(pval_funs) do
   pval_names[v] = k
end

-- interval names, indexed by "p/q" pairs

intnam_file = readfile(prefix .. "lib/intnam.par")
intnam = {}
interv = {}
for line in intnam_file:gmatch("([^\r\n]+)") do
   local k, v = line:match("^%s*([%d/]+)%s+(.+)")
   if k then
      intnam[k] = v
      table.insert(interv, scale_val(k))
   end
end

-- parse Scala file format

function info_val(s)
   if s:sub(1, 1) == "[" then
      -- For historical reasons, matrix data is stored in Pure list syntax,
      -- change it to Lua table syntax.
      s = s:gsub("%[","{"):gsub("%]","}")
   end
   local f = load("return " .. s)
   if f then
      return f()
   else
      return nil
   end
end

-- See http://www.huygens-fokker.org/scala/scl_format.html for a description
-- of the file format we parse here.
function parse_scala_file(data)
   local descr, n
   local scale = {}
   local info = {}
   for line in data:gmatch("([^\r\n]+)") do
      -- remove leading whitespace and skip empty lines and comments
      line = line:match("^%s*(.+)")
      if line then
	 if line:sub(1,1) ~= "!" then
	    if not descr then
	       -- the first non-comment line is the description
	       descr = line
	    elseif not n then
	       -- the next non-comment line is the number of scale points
	       n = tonumber(line)
	    else
	       -- Scala permits trailing comments after each scale point,
	       -- delimited by whitespace, we ignore those here.
	       for s in line:gmatch("[^%s]+") do
		  local ret, v = pcall(scale_val, s)
		  if ret then
		     table.insert(scale, v)
		  else
		     error("Scala format error")
		  end
		  break
	       end
	    end
	 elseif n then
	    -- Comment line after the scale data has been read. This data is
	    -- optional. If present, the data should be in additional comment
	    -- lines of the form '! id = val'. The supported values are 'M'
	    -- (disharmonicity metric), 'V' (3D embedding), 'PV' (prime
	    -- valuation function used to define the metric, 's' (stress-1
	    -- value of the embedding) and 'dmax' (maximum disharmonicity
	    -- value used as a threshold, e.g., when drawing the scale).
	    local k, v = line:match("^! (%w+) = (.+)")
	    if k then
	       info[k] = info_val(v)
	    end
	 end
      end
   end
   -- sanity checks
   if type(n) == "number" and #scale == n then
      -- Scala mandates that the start point 1/1 = 0.0 is implicit and should
      -- not be in the file, so add it back in. (The format spec doesn't
      -- require that scale points are in a particular order, so we just take
      -- the given order as is.)
      table.insert(scale, 1, {1,1})
      n = n+1
      -- Check that the info data is in good shape, otherwise we ignore it.
      local good = true
      local keys = { ["M"] = true, ["V"] = true, ["PV"] = true,
	 ["s"] = true, ["dmax"] = true }
      for k,v in pairs(info) do
	 if not keys[k] or
	    k == "M" and type(v) ~= "table" or
	    k == "V" and type(v) ~= "table" or
	    k == "dmax" and type(v) ~= "number" or
	    k == "s" and type(v) ~= "number" or
	    k == "PV" and type(v) ~= "function" or
	    k == "PV" and not pval_names[v]
	 then
	    good = false
	    break
	 end
      end
      if not good then
	 info = {}
      end
      if not next(info) then
	 local msg = "The scale hasn't been drawn yet.\nDo you wish to draw it now?"
	 local _
	 _, info = draw_if(msg, scale)
      else
	 info.scale = scale
      end
   else
      error("Scala format error")
   end
   return descr, scale, info
end

-- maintain scale descriptions and titles

function clear_scale_descr()
  tk.eval("$list erase 0 end")
  tk.eval("$metric erase 0 end")
  local n = tonumber(tk.get("maxcols"))
  for i = 2, n+1 do
     tk.eval(string.format("$metric columnConfigure %d -visible 0", i))
  end
end;

-- Attenuation factor for the Gaussian bell curve used to scale harmonicity
-- values, see below. 0.05 seems to be a resonable value, but you can change
-- this here as needed.
att = 0.05

-- weighted disharmonicity value: x0 = the nominal cent value; x = the
-- candidate interval, which must be rational; tol = the tuning tolerance in
-- cent; dmax = maximum disharmonicity; pv = the prime valuation function; the
-- computed result is the weighted harmonicity h/w, where h = hrm(x, pv), w =
-- exp (-d*d*ln (1/att)/(tol*tol)), d = abs(x0-cent(x)), if d<=tol and h <=
-- dmax, and nil otherwise.
function whrm(x0, x, tol, dmax, pv)
   local d = math.abs(cent(x0)-cent(x))
   if d <= tol then
      local h = hrm(x, pv)
      if h <= dmax then
	 local w = math.exp(-d*d*math.log(1/att)/(tol*tol))
	 return h/w
      else
	 return nil
      end
   else
      return nil
   end
end

function tonumber_inf(s)
   if s == "inf" then
      return inf
   else
      return tonumber(s)
   end
end

function scale_descr()
   local n = #state.scale
   local pv = state.PV
   local tol = tonumber(tk.get("tol"))
   local dmax = tonumber_inf(tk.get("dmax"))
   local nmax = tonumber(tk.get("nmax"))
   local info = {}
   for i = 1, n do
      local x = state.scale[i]
      local sel = type(x) == "table" and 1 or 0
      local cs = {}
      -- determine tuning candidates
      for _, y in ipairs(interv) do
	 local h = whrm(x, y, tol, dmax, pv)
	 if h then
	    table.insert(cs, { x = y, h = h })
	 end
      end
      -- sort cs by increasing weighted disharmonities
      table.sort(cs, function (a, b)
		    return a.h < b.h
      end)
      -- take the first nmax candidates
      cs = {table.unpack(cs, 1, nmax)}
      -- construct the candidate items (same contents as the main info
      -- structure minus the candidates list, see below)
      cs = map(cs, function (a)
		  return {0, i-1, a.x,facts (a.x), cent(a.x),
			  hrm(a.x, pv), name(a.x), descr(a.x)}
      end)
      local h = type(x) == "table" and hrm(x, pv) or 0.0
      info[i] = {sel,      -- selection status
		 i-1,      -- ordinal
		 x,        -- scale point (cent or rational)
		 facts(x), -- factors (if available)
		 cent(x),  -- interval in cent
		 h,        -- disharmonicity
		 name(x),  -- note name
		 descr(x), -- interval name (intnam)
		 cs}       -- candidate tunings
   end
   return info
end

function metric_descr()
   local n = #state.M
   assert(n==0 or n==#state.M[1])
   local info = {}
   for i = 1, n do
      info[i] = {i-1, state.scale[i], table.unpack(state.M[i])}
   end
   return info
end

function make_scale_descr()
   clear_scale_descr()
   local descr = scale_descr()
   if #descr == 0 then
      return
   end
   local mdescr = metric_descr()
   -- build the scale view
   --[[ This is ugly. Gnocl's tree widget doesn't give us any programmatic
      control over the order of the view, which might have been changed by the
      user. However, the rest of the code depends on that the rows are
      inserted in the proper order here. So we have to recreate the entire
      view to ensure that no custom order is in effect. ]]
   tk.eval("reinit_list")
   local items = {}
   local candidates = {}
   for i, d in ipairs(descr) do
      -- create a table row
      local sel, n, x, fs, ct, h, nm, iv, cs = table.unpack(d)
      items[i] = tk.join({tostring(sel), -- selection status
			  tostring(n),   -- ordinal
			  pscale(x),     -- scale point
			  fs,            -- factors
			  tostring(ct),  -- cent
			  tostring(h),   -- disharmonicity
			  nm,            -- note name
			  iv})           -- interval name (intnam)
      -- create children for each candidate
      candidates[i] = {}
      for j, c in ipairs(cs) do
	 local sel, m, x, fs, ct, h, nm, iv = table.unpack(c)
	 candidates[i][j] = tk.join({tostring(sel),
				     tostring(m),
				     pscale(x),
				     fs,
				     tostring(ct),
				     tostring(h),
				     nm,
				     iv})
      end
   end
   -- we can add all the items for a given parent in one chunk, which
   -- presumably is faster
   tk.eval(string.format("$list add {} {%s}", tk.join(items)))
   for i, items in ipairs(candidates) do
      tk.eval(string.format("$list add %d {%s}", i-1, tk.join(items)))
   end
   -- build the metric view
   if #mdescr == 0 then
      return
   end
   local scale = {}
   local k = #mdescr[1]
   local l = tonumber(tk.get("maxcols"))
   local padding = map(seq(k, l), function (i)
			  return 0.0
   end)
   local data = {}
   for i, d in ipairs(mdescr) do
      -- convert data to strings
      for j = 1, #d do
	 if j==2 then
	    -- d[2] is the scale point
	    d[j] = pscale(d[j])
	 else
	    -- everything else is just a number
	    d[j] = tostring(d[j])
	 end
      end
      -- keep track of the scale points, we need them again for the column
      -- headers
      scale[i] = d[2]
      -- zero padding, filling up to l (maxcol) columns
      if #d < l then
	 d = tableconcat(d, padding)
      elseif #d > l then
	 -- too many columns
	 d = {table.unpack(d, 1, l)}
      end
      data[i] = tk.join(d)
   end
   assert(#descr == #scale)
   tk.eval(string.format("$metric add {%s}", tk.join(data)))
   -- column headers
   for i = 2, k-1 do
      tk.eval(string.format("$metric columnConfigure %d -visible 1 -title {%s}", i, scale[i-1]))
   end
end

function get_title(name)
   tk.set("fname", name)
   return tk.eval("file tail $fname")
end

function set_title(name)
   tk.set("fname", name)
   tk.eval("$top configure -title \"[file tail $fname] - scale\"");
end

-- progress bar and dialogs

function name_plate()
   tk.eval(string.format("$progress configure -fraction 0 -text \"Scale Version %s\"", prog_version))
end

function info_dg(msg)
   if type(msg) ~= "string" then
      msg = "Unknown Error"
   end
   tk.set("msg", msg)
   tk.eval("info_dg $msg")
end

function error_dg(msg)
   tk.set("msg", msg)
   tk.eval("error_dg $msg")
end

function bad_scale_dg()
   if not bad_scale_data then
      bad_scale_data = ""
   end
   local msg = string.format(bad_scale_text, bad_scale_data)
   error_dg(msg)
end

function rat_error_dg()
   local msg = [[The rationalization routine raised an exception.

This is a bug. Please consider sending a bug report with the text of this message to the author at <span foreground=\"blue\" underline=\"single\">aggraef@gmail.com</span>."]]
   error_dg(msg)
end

-- Compute a scale factor for the node glyphs.

function scale_factor(V)
   if #V == 0 then
      return 1.0
   end
   local xs = map(seq(1, #V), function (i)
		     return V[i][1]
   end)
   local ys = map(seq(1, #V), function (i)
		     return V[i][2]
   end)
   local zs = map(seq(1, #V), function (i)
		     return V[i][3]
   end)
   local xmin = math.min(table.unpack(xs))
   local xmax = math.max(table.unpack(xs))
   local ymin = math.min(table.unpack(ys))
   local ymax = math.max(table.unpack(ys))
   local zmin = math.min(table.unpack(zs))
   local zmax = math.max(table.unpack(zs))
   local dx, dy, dz = xmax-xmin, ymax-ymin, zmax-zmin
   local d = math.max(dx, dy, dz)
   if d <= 0.0 then
      d = 1.0
   end
   return d
end

-- program state

state0 = { M = {}, V = {}, s = 0.0, dmax = 0.0, PV = barlow, scale = {} }
state = state0

bad_scale_text = [[Bad scale syntax: %s

Please enter a list of ratios (1/1, 3/2, etc.) and cent values (0.0, 100.0 etc.) into the scale field.

Example: 1/1 100.0 200.0 5/4 4/3 3/2 2/1]]

function new_scale()
   state = state0
   expanded = {}
   local data,wmin,wmax = vtk_data(state.dmax, state.V, state.M)
   tk.set("scale", "")
   tk.set("descr", "")
   tk.set("data", data)
   tk.set("wmin", wmin)
   tk.set("wmax", wmax)
   tk.set("dmax", state.dmax)
   tk.set("fact", "1.0")
   tk.set("lbls", "")
   tk.set("title", "")
   clear_scale_descr()
   set_title("Unnamed")
   return true
end

function read_scale(gui, name)
   local res, data = pcall(readfile, name)
   if not res then
      local msg = string.format("Error reading %s", name)
      if gui then
	 error_dg(msg)
      else
	 io.stderr:write(string.format("scale: %s\n", msg))
      end
      return false
   end
   local res, descr, scale, info = pcall(parse_scala_file, data)
   if not res then
      local msg = string.format("Error parsing %s", name)
      if gui then
	 error_dg(msg)
      else
	 io.stderr:write(string.format("scale: %s\n", msg))
      end
      return false
   end
   state = info
   expanded = {}
   local pv_name = pval_names[state.PV]
   if not pv_name then
      -- bad pval function in info; change to default
      state.PV = barlow
      pv_name = pval_names[state.PV]
      assert(pv_name)
   end
   recursive = true
   tk.set("pval", pv_name)
   local data,wmin,wmax = vtk_data(state.dmax, state.V, state.M)
   tk.set("scale", unparse_scale(scale))
   tk.set("descr", descr)
   tk.set("data", data)
   tk.set("wmin", wmin)
   tk.set("wmax", wmax)
   tk.set("dmax", state.dmax)
   tk.set("fact", scale_factor(state.V))
   make_scale_descr()
   tk.set("title", string.format("stress = %g", state.s))
   local lbls = node_labels(tk.get("node_labels"), tk.get("label_type"))
   tk.set("lbls", tk.join(lbls))
   set_title(name)
   recursive = false
   return true
end

function mts_convert(t, rt, b, r)
   -- shift the tuning so that the reference tone is at 0 cent
   r = (r<0 and 0) or (r>11 and 11) or r
   local t0 = t[r+1]
   for i = 1, #t do
      t[i] = t[i] - t0
   end
   -- encode the tuning
   local u = {}
   u[1] = 0x7e + (rt and 1 or 0) -- realtime / non-realtime
   u[2] = 0x7f -- device id (any device)
   u[3] = 8 -- MTS tuning
   u[4] = 8 + (b and 1 or 0) -- 1/2 byte octave-based
   u[5] = 3; u[6] = 0x7f; u[7] = 0x7f -- MIDI channel mask (all channels)
   if b then
      -- In the 2-byte encoding the value denotes a single 14 bit number in
      -- the range 0..16383 (most significant byte first). 0 denotes -100
      -- cent, 8192 is the center (0 cent) and 16384 would be +100 cent
      -- (corresponding to an effective resolution of 100/2^14 == .012207
      -- cent).
      local function val(x)
	 local y = math.floor(x*8192.0/100.0+0.5) + 8192
	 y = (y < 0 and 0) or (y > 16383 and 16383) or y
	 local msb = y >> 7
	 local lsb = y - (msb << 7)
	 return msb, lsb
      end
      for i = 1, #t do
	 local msb, lsb = val(t[i])
	 u[2*i+6] = msb
	 u[2*i+7] = lsb
      end
   else
      -- In the 1-byte encoding we simply have cent values in the range
      -- 0..127 where 64 is the center (0 cents).
      local function val(x)
	 local y = math.floor(x+0.5) + 64
	 y = (y < 0 and 0) or (y > 127 and 127) or y
	 return y
      end
      for i = 1, #t do
	 u[i+7] = val(t[i])
      end
   end
   return u
end

function save_sysex(name)
   local ret, scale = pcall(parse_scale)
   if not ret then
      bad_scale_dg()
      return
   end
   if #scale ~= 13 then
      local msg = "<b>Not a 12-tone scale.</b> To save a tuning in one of the MTS octave-based formats, the scale must have exactly 12 tones plus the octave (2/1)."
      error_dg(msg)
      return
   end
   if cent(scale[#scale]) ~= 1200.0 then
      local msg = "<b>Not an octave scale.</b> To save a tuning in one of the MTS octave-based formats, the scale must have exactly 12 tones plus the octave (2/1)."
      error_dg(msg)
      return
   end
   local res = tk.eval("mts_dg")
   if res ~= "Save" then
      return
   end
   local realtime = tk.get("mts_realtime") == "1"
   local encoding = tk.get("mts_encoding") == "2-byte"
   local basetone = tonumber(tk.get("mts_basetone"))
   -- convert to cent values and compute the tuning offsets
   local t = map({table.unpack(scale, 1, #scale-1)}, cent)
   assert(#t == 12)
   for i = 1, 12 do
      t[i] = t[i] - (i-1)*100.0
   end
   local data = mts_convert(t, realtime, encoding, basetone)
   -- add F0 ... F7 for sysex data
   data = string.char(0xf0) ..
      string.char(table.unpack(data)) ..
      string.char(0xf7)
   local f = io.open(name, "wb")
   if not f or not f:write(data) then
      local msg = string.format("Error writing %s", name)
      error_dg(msg)
   end
   if f then f:close() end
end

function save_scale(name)
   local ret, scale = pcall(parse_scale)
   if ret then
      while scale[1] == 0.0 or pscale(scale[1]) == "1/1" do
	 table.remove(scale, 1)
      end
   else
      bad_scale_dg()
      return
   end
   local descr = tk.get("descr")
   local pv = pval_funs[tk.get("pval")]
   local M = inspect(state.M)
   local V = inspect(state.V)
   local s = state.s
   local dmax = state.dmax
   assert(pv)
   -- convert M and V to Pure list format for compatibility with the Pure
   -- version of the scale program
   M = M:gsub("{","["):gsub("}","]"):gsub(" ","")
   V = V:gsub("{","["):gsub("}","]"):gsub(" ","")
   local data = string.format([[! %s
!
%s
%d
!
%s
!!! information added by scale program -- DO NOT EDIT !!!
! PV = %s
! M = %s
! V = %s
! s = %s
! dmax = %s
]],
      get_title(name), descr, #scale,
      unparse_scale(scale, "\n"),
      pval_fun_names[pv], M, V, tostring(s), tostring(dmax))
   local f = io.open(name, "w")
   if not f or not f:write(data) then
      local msg = string.format("Error writing %s", name)
      error_dg(msg)
      if f then f:close() end
      return
   end
   f:close()
   -- Update the state information.
   table.insert(scale, 1, {1,1})
   tk.set("scale", unparse_scale(scale))
   state.PV = pv
   state.scale = scale
   set_title(name)
end

function draw_scale()
   local ret, scale = pcall(parse_scale)
   if not ret then
      bad_scale_dg()
      return false
   end
   local dmax = tonumber_inf(tk.get("dmax"))
   local pv = pval_funs[tk.get("pval")]
   assert(pv)
   -- Compute the embedding.
   tk.eval("$status push {Drawing scale, please wait...}")
   tk.eval("$drawbut configure -sensitive 0")
   local M,V,s = embed_scale(pv, scale)
   tk.eval("$drawbut configure -sensitive 1")
   tk.eval("$status pop")
   -- Update the state information.
   state = {["M"]=M,["V"]=V,["s"]=s,["dmax"]=dmax,["PV"]=pv,["scale"]=scale}
   expanded = {}
   -- Redraw.
   local data,wmin,wmax = vtk_data(dmax, V, M)
   local fact = scale_factor(V)
   tk.set("data", data)
   tk.set("wmin", wmin)
   tk.set("wmax", wmax)
   tk.set("dmax", dmax)
   tk.set("fact", fact)
   make_scale_descr()
   tk.set("title", string.format("stress = %g", s))
   lbls = node_labels(tk.get("node_labels"), tk.get("label_type"));
   tk.set("lbls", tk.join(lbls))
   return true
end

function draw_if(msg, scale)
   local res = tk.eval(string.format("question_dg \"%s\"", msg))
   if res ~= "Yes" then
      local res = state0
      res.scale = scale
      return false, res
   end
   local pv = pval_funs[tk.get("pval")]
   assert(pv)
   tk.eval("$status push {Drawing scale, please wait...}")
   tk.eval("$drawbut configure -sensitive 0")
   local M,V,s = embed_scale(pv, scale)
   tk.eval("$drawbut configure -sensitive 1")
   tk.eval("$status pop")
   local dmax = tonumber_inf(tk.get("dmax"))
   local res = {["M"]=M,["V"]=V,["s"]=s,["dmax"]=dmax,["PV"]=pv,["scale"]=scale}
   return true, res
end

function mds(pv, scale)
   local M = hrm_scale(scale, pv)
   local V, s = oct.feval("mds", 2, M, 3)
   return M, V, s
end

function embed_scale(pv, scale)
   local n = #scale
   local israt = true
   local points = {}
   for i, x in ipairs(scale) do
      if type(x) == "number" then
	 israt = false
      end
      points[i] = { i = i, x = cent(x) }
   end
   if israt then
      start_log()
      local M, V, s = mds(pv, scale)
      end_log()
      return M, V, s
   else
      -- We have irrational scale points, just render them as a chain in
      -- ascending order.
      table.sort(points, function (a, b)
		    return a.x < b.x or a.x == b.x and a.i < b.i
      end)
      for i = 1, n do
	 points[i].p = { (i-1)/(n-1), 0.0, 0.0 }
      end
      table.sort(points, function (a, b)
		    return a.i < b.i
      end)
      local V = map(points, function (a)
		       return a.p
      end)
      local M = {}
      return M, V, 0.0
   end
end

function vtk_data(dmax, V, M)
   local l = #V
   local n = #M
   local m = n>0 and #M[1] or 0
   -- blank-delimited list of all point coordinates of the embedding
   local points = table.concat(
      map(V, function (x)
	     return table.concat(map(x, tostring), " ")
      end), " ")
   -- newline-delimited list of all edges with weights <= dmax
   local edges, weights, types, w = {}, {}, {}, {}
   for i = 1, n do
      for j = 1, m do
	 if i ~= j and M[i][j] <= dmax then
	    -- VTK expects zero-based point indices here
	    table.insert(edges, string.format("2 %d %d", i-1, j-1))
	    table.insert(weights, tostring(M[i][j]))
	    table.insert(types, "3")
	    table.insert(w, M[i][j])
	 end
      end
   end
   local k = #edges
   edges = table.concat(edges, "\n")
   weights = table.concat(weights, "\n")
   types = table.concat(types, "\n")
   local data = string.format([[# vtk DataFile Version 2.0
Unstructured Grid Example
ASCII

DATASET UNSTRUCTURED_GRID
POINTS %d float
%s
CELLS %d %d
%s
CELL_TYPES %d
%s
CELL_DATA %d
SCALARS weights float
LOOKUP_TABLE default
%s
]], l, points, k, 3*k, edges, k, types, k, weights)
   -- determine min and max weights
   if #w == 0 then
      w = {0.0, 1.0}
   elseif #w == 1 then
      w = {0.0, w[1]}
   end
   table.sort(w)
   local wmin, wmax = w[1], w[#w]
   return data, wmin, wmax
end

-- Callbacks

-- create a new, and load/save an existing scale

function new_cb()
   if new_scale() then
      tk.eval("new_file \"\"; reload_data 1; renWin Render")
   end
end

function open_cb()
   local name = tk.eval("open_dg")
   if name and name ~= "" then
      tk.eval("$status push {Loading scale...}")
      tk.set("fname", name)
      if read_scale(true, name) then
	 tk.eval("new_file $fname; reload_data 1; renWin Render")
      end
      tk.eval("$status pop")
   end
end

function revert_cb()
   local name = tk.get("lastfile")
   if name and name ~= "" then
      tk.eval("$status push {Reverting scale...}")
      if read_scale(true, name) then
	 tk.eval("reload_data 1; renWin Render")
      end
      tk.eval("$status pop")
   end
end

function save_cb()
   local name = tk.eval("save_dg")
   if name and name ~= "" then
      tk.eval("$status push {Reverting scale...}")
      local path, name, ext = name:match("(.-)([^\\/]-%.?([^%.\\/]*))$")
      if ext == "syx" then
	 save_sysex(name)
      else
	 save_scale(name)
	 tk.set("fname", name)
	 tk.eval("new_file $fname")
      end
      tk.eval("$status pop")
   end
end

-- logging (progress bar)

last_s = 0.0

function start_log()
   last_s = 0.0
   oct.set("logging", 1)
end

function end_log()
   name_plate()
   oct.set("logging", 0)
end

function log_cb(n, s)
   local old, new = last_s, s
   last_s = s
   local p = old - new
   p = p == 0.0 and 1.0 or
      math.min(1.0, math.max(0.0, math.log(100.0/math.abs(p))/math.log(10)/8))
   p = math.floor(p*100+0.5)/100
   tk.eval(string.format("$progress configure -fraction %.4f -text \"%.0f%%\"",
			 p, p*100))
end

function log2_cb(p)
   tk.eval(string.format("$progress configure -fraction %.4f -text \"%.0f%%\"",
			 p, p*100))
end

-- rationalize and draw a scale

function draw_cb()
   if draw_scale() then
      tk.eval("reload_data 1; renWin Render")
   end
end

function rat_scale(scale)
   local n = #scale
   if tonumber(tk.eval("$list getNumChildren {}")) ~= n then
      return -1
   end
   local pv = state.PV
   local dmax = state.dmax
   local heur = tk.get("heur")
   -- Get the selected candidate tunings.
   local T = {}
   local T0 = {} -- fixed tunings
   local have_any = false -- whether there are any nontrivial candidate sets
   local ms = {} -- children count
   for i = 1, n do
      T[i] = {}
      local sel =
	 tonumber(tk.eval(string.format("$list get %d 0", i-1)))
      local x = scale[i]
      -- assert(x == scale_val(tk.eval(string.format("$list get %d 2", i-1))))
      local m = tonumber(tk.eval(string.format("$list getNumChildren %d", i-1)))
      ms[i] = m
      if sel ~= 0 and type(x) == "table" then
	 -- fixed tuning
	 T0[i] = x
      else
	 local k = 0
	 -- we only consider candidates for tunings which aren't fixed
	 for j = 1, m do
	    sel =
	       tonumber(tk.eval(string.format("$list get {%d %d} 0", i-1, j-1)))
	    if sel ~= 0 then
	       x = scale_val(tk.eval(string.format("$list get {%d %d} 2", i-1, j-1)))
	       T[i][j] = x
	       k = k+1
	    end
	 end
	 if k > 1 then
	    have_any = true
	 end
      end
   end
   if not have_any then
      -- there's nothing to do
      return -1
   end
   -- get rid of all candidates which exceed the weight thresholds for any
   -- of the fixed tunings
   for i = 1, n do
      for j, x in pairs(T[i]) do
	 for _, y in pairs(T0) do
	    if hrm_dist(x, y, pv) > dmax then
	       T[i][j] = nil
	       goto continue
	    end
	 end
	 ::continue::
      end
   end
   -- keep track of the real node indices and put the candidate sets into
   -- contiguous lists; this simplifies the Lua version of the algorithm
   local index = {}
   for i = 1, n do
      local V = {}
      index[i] = {}
      for j, x in pairs(T[i]) do
	 table.insert(V, x)
	 table.insert(index[i], j)
      end
      T[i] = V
   end
   -- Perform the rationalization.
   local ret, C = xpcall(rationalize, rat_error_dg, T, heur, dmax, pv)
   if not ret then
      return 0
   end
   -- retrieve the real node indices
   C = map(C, function (v)
	      local i, j, x = table.unpack(v)
	      j = index[i][j]; assert(j)
	      return {i, j, x}
   end)
   end_log()
   if #C == 0 then
      -- Nothing found (most likely we were interrupted).
      return 0
   end
   -- Update the selection.
   for i = 1, n do
      for j = 1, ms[i] do
	 tk.eval(string.format("$list cellConfigure {%d %d} 0 -value 0", i-1, j-1))
      end
   end
   for _, v in ipairs(C) do
      local i, j, x = table.unpack(v)
      tk.eval(string.format("$list cellConfigure {%d %d} 0 -value 1", i-1, j-1))
   end
   return #C
end

function rat_cb()
   local ret, scale = pcall(parse_scale)
   if not ret then
      bad_scale_dg()
      return
   end
   if unparse_scale(scale) ~= unparse_scale(state.scale) then
      error_dg("The scale has been edited. Please first do a Refresh to update the scale information.")
      return
   end
   local n = #scale
   if n == 0 then
      error_dg("Empty scale. You need to load or enter a scale first.")
      return
   end
   breakflag = false
   tk.eval("$status push {Rationalizing scale, please wait... (hit Interrupt to stop)}")
   tk.eval("$ratbut configure -text \"%_In_terrupt\" -onClicked {lua escape_cb} -tooltip \"Stop the ongoing computation\"")
   local res = rat_scale(scale)
   tk.eval("$ratbut configure -text \"%_Ra_tionalize\" -onClicked {lua rat_cb} -tooltip \"Rationalize scale\"")
   tk.eval("$status pop")
   local fin = breakflag and "interrupted" or "finished"
   breakflag = false
   -- Give some hopefully helpful diagnostics about the result.
   function sel1(i)
      local sel = tonumber(tk.eval(string.format("$list get %d 0", i)))
      return sel ~= 0
   end
   function sel(i)
      function sel2(j)
	 local sel =
	    tonumber(tk.eval(string.format("$list get {%d %d} 0", i, j)))
	 return sel ~= 0
      end
      local m =
	 tonumber(tk.eval(string.format("$list getNumChildren %d", i)))
      return sel1(i) or any(seq(0, m-1), sel2)
   end
   local msg
   if res == 0 then
      msg = string.format("%%<<b>Rationalization %s, no solutions were found.</b>\n\nYou might want to increase the weight threshold to enable larger harmonic distances and/or additional candidate tunings.", fin)
      error_dg(msg)
      return
   elseif res < 0 then
      local none = all(seq(0, n-1), sel1)
      msg = none and
      "The scale is already rational, so there is nothing to do. To compute an alternative rationalization, you should first unselect some scale points and select some alternative tunings. Then run <i>Rationalize</i> again." or
      "There's nothing to do, because at most one tuning alternative was selected for each scale point. You might want to generate and/or select some additional candidate tunings first."
      error_dg(msg)
      return
   end
   local done = all(seq(0, n-1), sel)
   if done then
      msg = "%%<<b>Rationalization %s, %d scale points were rationalized.</b>\n\nThe entire scale has been rationalized, so after reviewing the results you may now proceed with <i>Update</i> to update and redraw the scale. To perform a new rationalization, just select some new tuning alternatives and run <i>Rationalize</i> again."
   else
      msg = "%%<<b>Rationalization %s, %d scale points were rationalized.</b>\n\nSome scale points are still not rationalized, so you might have to select some more tuning alternatives, or try to increase the weight threshold to enable larger harmonic distances and/or additional candidate tunings. You may also proceed with <i>Update</i> to update the scale and fix the partial rationalization obtained so far."
   end
   info_dg(string.format(msg, fin, res))
end

breakflag = false

function escape_cb()
   print("Break...")
   breakflag = true
end

function update_cb()
   local ret, scale = pcall(parse_scale)
   if not ret then
      bad_scale_dg()
      return
   end
   if unparse_scale(scale) ~= unparse_scale(state.scale) then
      error_dg("The scale has been edited. Please first do a Refresh to update the scale information.")
      return
   end
   -- Check selections in the Scale list and update the scale accordingly.
   scale = update_scale(scale)
   -- Redraw the scale if anything has changed.
   if unparse_scale(scale) ~= unparse_scale(state.scale) and draw_scale() then
      tk.eval("reload_data 1; renWin Render")
   end
end

--  Get the selected scale points and update the scale accordingly.
function update_scale(scale)
   local n = #scale
   if tonumber(tk.eval("$list getNumChildren {}")) ~= n then
      return scale
   end
   -- create a shallow copy
   scale = {table.unpack(scale)}
   for i = 0, n-1 do
      -- check row #i
      local sel = tonumber(tk.eval(string.format("$list get %d 0", i)))
      if sel ~= 0 then
	 -- we already got scale[i+1], and it's selected, nothing to do
	 goto continue
      end
      -- check the children of row #i
      local m = tonumber(tk.eval(string.format("$list getNumChildren %d", i)))
      for j = 0, m-1 do
	 sel = tonumber(tk.eval(string.format("$list get {%d %d} 0", i, j)))
	 if sel ~= 0 then
	    -- found a selected item among the candidates, get its pitch
	    local x = scale_val(tk.eval(string.format("$list get {%d %d} 2", i, j)))
	    scale[i+1] = x
	    -- we pick the first selected item only, so we can bail out and
	    -- look at the next row at this point
	    goto continue
	 end
      end
      ::continue::
   end
   tk.set("scale", unparse_scale(scale))
   return scale
end

-- various status updates

function refresh_cb()
   local ret, scale = pcall(parse_scale)
   if not ret then
      bad_scale_dg()
      return
   end
   -- remember which scale items are currently expanded
   local old_expanded = expanded
   local n = tonumber(tk.eval("$list getNumChildren {}"))
   for k, _ in pairs(old_expanded) do
      if k > n then
	 -- remove stale entries
	 old_expanded[k] = nil
      end
   end
   make_scale_descr()
   -- expanded is empty now, restore it
   expanded = old_expanded
   for k, _ in pairs(expanded) do
      tk.eval(string.format("$list expand -path %d", k))
   end
   -- redraw the scale if it has changed
   if unparse_scale(scale) ~= unparse_scale(state.scale) then
      draw_cb()
   end
end

function select_all_cb()
   local n = tonumber(tk.eval("$list getNumChildren {}"))
   for i = 0, n-1 do
      local sel = tonumber(tk.eval(string.format("$list get %d 0", i)))
      if sel == 0 then
	 local m = tonumber(tk.eval(string.format("$list getNumChildren %d", i)))
	 for j = 0, m-1 do
	    tk.eval(string.format("$list cellConfigure {%d %d} 0 -value 1", i, j))
	 end
	 if m > 0 then
	    tk.eval(string.format("$list expand -path %d", i))
	 end
      end
   end
end

function unselect_all_cb()
   local n = tonumber(tk.eval("$list getNumChildren {}"))
   for i = 0, n-1 do
      tk.eval(string.format("$list cellConfigure %d 0 -value 0; $list collapse -path %d", i, i))
      local m = tonumber(tk.eval(string.format("$list getNumChildren %d", i)))
      for j = 0, m-1 do
	 tk.eval(string.format("$list cellConfigure {%d %d} 0 -value 0", i, j))
      end
   end
end

expanded = {}

function expand_cb(p)
   expanded[tonumber(p)] = true
end

function collapse_cb(p)
   expanded[tonumber(p)] = nil
end

recursive = false

function dmax_cb(v)
   local dmax = tonumber_inf(v)
   if recursive or not dmax or dmax == state.dmax then
      return
   end
   recursive = true
   local data,wmin,wmax = vtk_data(dmax, state.V, state.M)
   tk.set("data", data)
   tk.set("wmin", wmin)
   tk.set("wmax", wmax)
   tk.set("dmax", dmax)
   -- Update the state information.
   state.dmax = dmax
   last_dmax = dmax
   recursive = false
   tk.eval("reload_data 0; renWin Render")
end

function pval_cb(v)
   if recursive or v == pval_names[state.PV] then
      return
   end
   local ret, scale = pcall(parse_scale)
   if not ret then
      bad_scale_dg()
      return
   end
   local dmax = tonumber_inf(tk.get("dmax"))
   local msg = "The weight function has been changed.\nDo you wish to redraw the scale now?"
   local res, info = draw_if(msg, scale)
   if res then
      local data,wmin,wmax = vtk_data(dmax, info.V, info.M)
      local fact = scale_factor(info.V)
      tk.set("data", data)
      tk.set("wmin", wmin)
      tk.set("wmax", wmax)
      tk.set("dmax", dmax)
      tk.set("fact", fact)
      tk.set("title", string.format("stress = %g", info.s))
      state = info
      expanded = {}
      make_scale_descr()
      tk.eval("reload_data 1; renWin Render")
   end
end

function node_labels_cb(v, t)
   local lbls = node_labels(v, t)
   tk.set("lbls", tk.join(lbls))
   tk.eval("captions $lbls; renWin Render")
end

function edge_labels_cb(v)
   tk.eval(string.format("edge_labels SetVisibility %s; renWin Render", v))
end

function axes_cb(v)
   tk.eval(string.format("axes SetVisibility %s; renWin Render", v))
end

function interactor_cb(v)
   local s = tonumber(v) ~= 0 and "Trackball" or "Joystick"
   tk.eval(string.format("[[renWin GetInteractor] GetInteractorStyle] SetCurrentStyleTo%sCamera", s))
end

-- key handler on the graph view

function key_cb(mod, keysym)
   mod = tonumber(mod)
   if mod & bit(3) ~= 0 then
      -- Alt key combinations, used by the GUI.
      return
   end
   if keysym == "Escape" then
      escape_cb()
   elseif keysym == "r" then
      tk.eval("reset_camera; renWin Render")
   elseif keysym == "t" then
      interactor_cb("1")
   elseif keysym == "j" then
      interactor_cb("0")
   end
end

-- stuff to do at exit

function fini_cb()
   print("Exiting...")
end

-- about dialog and help

about_text = string.format(
   [[This is <b>scale version %s</b>, a program for musical scale visualization and rationalization.

Copyright (c) 2010-2022 by Albert Gräf, <span foreground="blue" underline="single">aggraef@gmail.com</span>. The source of this program is available from GitHub, see <span foreground="blue" underline="single">https://github.com/agraef/scale-lua</span>.

The scale program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

The scale program is distributed in the hope that it will be useful, but <b>without any warranty</b>; without even the implied warranty of <b>merchantability</b> or <b>fitness for a particular purpose</b>.

Please see the COPYING file accompanying this program for the precise license terms. The GPL can also be read online at <span foreground="blue" underline="single">http://www.gnu.org/licenses</span>.]],
prog_version)

function about_cb()
  tk.eval("about_dg");
end

function help_cb()
  tk.eval("help_dg");
end

--[[ The main program: execute the Tcl script and enter the main loop. ]]

function main()
   tk.eval("wm withdraw .")
   tk.set("about_text", about_text)
   prog = arg[0] and arg[0] or "scale"
   if #arg > 0 and arg[1] == "-g" then
      -- show graph in a separate window
      tk.eval(string.format("set GRAPHWIN %d", 1))
      table.remove(arg, 1)
   end
   fname = arg[1] and arg[1] or ""
   tk.set("argv0", prog)
   -- load the GUI
   tk.eval(scaleprog)
   tk.set("fname", fname)
   tk.eval("new_file $fname")
   name_plate()
   -- initialize the scale
   res = fname ~= "" and read_scale(false, fname) or new_scale()
   if res and fname ~= "" then
      tk.eval("reload_data 1; renWin Render")
   end
   -- enter the main loop
   tk.main()
end

main()
