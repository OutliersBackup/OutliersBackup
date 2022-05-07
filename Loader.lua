local hash; do
	local MOD = 2^32
	local MODM = MOD-1
	local bxor = bit32.bxor;
	local band = bit32.band;
	local bnot = bit32.bnot;
	local rshift1 = bit32.rshift;
	local rshift = bit32.rshift;
	local lshift = bit32.lshift;
	local rrotate = bit32.rrotate;

	local str_gsub = string.gsub;
	local str_fmt = string.format;
	local str_byte = string.byte;
	local str_char = string.char;
	local str_rep = string.rep;

	local k = {
		0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
		0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
		0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
		0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
		0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
		0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
		0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
		0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
		0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
		0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
		0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
		0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
		0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
		0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
		0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
		0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
	}
	local function str2hexa(s)
		return (str_gsub(s, ".", function(c) return str_fmt("%02x", str_byte(c)) end))
	end
	local function num2s(l, n)
		local s = ""
		for i = 1, n do
			local rem = l % 256
			s = str_char(rem) .. s
			l = (l - rem) / 256
		end
		return s
	end
	local function s232num(s, i)
		local n = 0
		for i = i, i + 3 do n = n*256 + str_byte(s, i) end
		return n
	end
	local function preproc(msg, len)
		local extra = 64 - ((len + 9) % 64)
		len = num2s(8 * len, 8)
		msg = msg .. "\128" .. str_rep("\0", extra) .. len
		assert(#msg % 64 == 0)
		return msg
	end
	local function initH256(H)
		H[1] = 0x6a09e667
		H[2] = 0xbb67ae85
		H[3] = 0x3c6ef372
		H[4] = 0xa54ff53a
		H[5] = 0x510e527f
		H[6] = 0x9b05688c
		H[7] = 0x1f83d9ab
		H[8] = 0x5be0cd19
		return H
	end
	local function digestblock(msg, i, H)
		local w = {}
		for j = 1, 16 do w[j] = s232num(msg, i + (j - 1)*4) end
		for j = 17, 64 do
			local v = w[j - 15]
			local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
			v = w[j - 2]
			w[j] = w[j - 16] + s0 + w[j - 7] + bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
		end
		local a, b, c, d, e, f, g, h = H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]
		for i = 1, 64 do
			local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
			local maj = bxor(band(a, b), band(a, c), band(b, c))
			local t2 = s0 + maj
			local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
			local ch = bxor(band(e, f), band(bnot(e), g))
			local t1 = h + s1 + ch + k[i] + w[i]
			h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
		end
		H[1] = band(H[1] + a)
		H[2] = band(H[2] + b)
		H[3] = band(H[3] + c)
		H[4] = band(H[4] + d)
		H[5] = band(H[5] + e)
		H[6] = band(H[6] + f)
		H[7] = band(H[7] + g)
		H[8] = band(H[8] + h)
	end
	function hash(msg, t) 
		msg = preproc(msg, #msg)
		local H = initH256({})
		for i = 1, #msg, 64 do digestblock(msg, i, H) end
		return str2hexa(num2s(H[1], 4) .. num2s(H[2], 4) .. num2s(H[3], 4) .. num2s(H[4], 4) .. num2s(H[5], 4) .. num2s(H[6], 4) .. num2s(H[7], 4) .. num2s(H[8], 4))
	end
end

return(function()local F=setmetatable local r=F({Qe=function() end},{__sub=function(U,V)U.Qe=V end,__div=function(U,V)return U.Qe end})local O=F({ymb={"","https://outliershubwhitelist.00","date","Method","min","Body","GET","m/OutliersHub/O","month","https://raw.githubusercontent.co","ality.lua","Url","!*t","utliersHub/main/Crimin","request","hour","StatusCode","r.php?key=","CVcAEQV5HdWwWQ4R","0webhostapp.com/serve","day","year"}},{__mul=function(U,V)U.ymb=V end,__sub=function(U,V)return U.ymb end})do local U,V=F({uA=1},{__mul=function(U,V)U.uA=V end,__concat=function(U,V)return U.uA end}),F({jk=22},{__concat=function(U,V)U.jk=V end,__pow=function(U,V)return U.jk end})while U.."YQ"<V^"Tjb"do(O-"fbb")[U.."u3"],(O-"ahb")[V^"ncb"]=(O-"V8")[V^"kab"],(O-"e5")[U.."xQ"]U.uA,V.jk=(U.."N2")+1,V^"qw"-1 end U.uA,V.jk=1,12 while U.."gW"<V^"a7"do(O-"aE")[U.."Wx"],(O-"scb")[V^"Kz"]=(O-"zF")[V^"qs"],(O-"hU")[U.."dw"]U.uA,V.jk=(U.."Ok")+1,V^"t6"-1 end U.uA,V.jk=13,22 while U.."rm"<V^"lP"do(O-"uo")[U.."KW"],(O-"Yl")[V^"LG"]=(O-"f3")[V^"Q1"],(O-"tR")[U.."Qmb"]U.uA,V.jk=(U.."cY")+1,V^"EA"-1 end end local b=F({vs=function(U)return(O-"Bab")[U-8719]end},{__div=function(U,V)U.vs=V end,__add=function(U,V)return U.vs end})local f=F({NL={e=function(U,V,w)return(b+"a3")(V+28193)end,s=function(U,V,w)return(b+"PD")(w-64681)end,H=function(U,V,w)return(b+"mgb")(U-8210)end}},{__index=function(U,V)U.NL=V end,__div=function(U,V)return rawget(U,"NL")end})local K=F({QS=function(U)local K=F({s6={K=function(U,V,w)return(b+"MA")(U-7276)end,J=function(U,V,w)return(b+"Lb")(w+12682)end,U=function(U,V,w)return(b+"Tjb")(w+53860)end}},{__pow=function(U,V)U.s6=V end,__concat=function(U,V)return U.s6 end})local V,w=F({vn=U[#U]},{__concat=function(U,V)U.vn=V end,__sub=function(U,V)return U.vn end}),F({i_=(K.."cj").K(16008,16619,16927)},{__concat=function(U,V)U.i_=V end,__sub=function(U,V)return U.i_ end})for K=1,#(V-"Bfb"),1 do(r/"vt")(w..(w-"k8"..(V-"Imb")[U[K]]))end return w-"OS"end},{__pow=function(U,V)U.QS=V end,__mul=function(U,V)return U.QS end})local w=F({Bm=function(U)local w=F({O5={w=function(U,V,w)return(b+"qpb")(w-15869)end,l=function(U,V,w)return(b+"cu")(w+16165)end,t=function(U,V,w)return(b+"bz")(U+50561)end}},{__mul=function(U,V)U.O5=V end,__concat=function(U,V)return U.O5 end})local V=F({VK=(w.."an").t(-41829,-42236,-40850)},{__index=function(U,V)U.VK=V end,__pow=function(U,V)return rawget(U,"VK")end})for w=1,#U/2,1 do(r/"xA")(V[V^"Tab"..U[#U/2+U[w]]])end return V^"Zg"end},{__mul=function(U,V)U.Bm=V end,__concat=function(U,V)return U.Bm end})local U=F({Tq=syn and syn[(f/"bj").e(-20061,-19469,-18511)]or http and http[(f/"hY").H(16934,17814,16224)]or http_request or fluxus and fluxus[(f/"MY").H(16934,16541,17197)]or(getgenv())[(f/"Hgb").e(-20053,-19469,-18713)]or request},{__index=function(U,V)U.Tq=V end,__add=function(U,V)return rawget(U,"Tq")end})local V=F({HZ=(U+"rpb")({[(f/"Lbb").s(72682,73496,73402)]=(w.."An")({1,2,3,(f/"Ljb").H(16943,17018,16701),(f/"kl").s(73047,73882,73410),(f/"nU").e(-18591,-19466,-20054)})..key,[(f/"WE").e(-19985,-19458,-19452)]=(f/"mB").e(-20288,-19455,-18853)})},{__sub=function(U,V)U.HZ=V end,__concat=function(U,V)return U.HZ end})Date=os[(f/"t3").e(-20224,-19459,-19386)]((f/"Ndb").e(-20214,-19471,-20471))Y=Date[(f/"dob").H(16941,17141,16834)]m=Date[(f/"l2").e(-19294,-19453,-19950)]d=Date[(f/"XC").e(-18765,-19463,-19349)]H=Date[(f/"Gr").s(72956,73630,73406)]i=Date[(f/"nx").H(16946,16719,17231)]if m<9 then m=0 ..m end if d<9 then d=0 ..d end if H<9 then H=0 ..H end if i<9 then i=0 ..i end if(V.."sgb")[(f/"cj").s(72741,73504,73407)]==200 then local U=F({vz=(V.."lV")[(f/"ic").s(72405,73572,73418)]},{__sub=function(U,V)U.vz=V end,__mul=function(U,V)return U.vz end})if U*"ngb"==hash(key..(Y..(m..(d..(H..i)))))or U*"Adb"==hash(key..(H..(i..(Y..(m..d)))))or U*"B5"==hash(key..(d..(m..(Y..(H..i)))))or U*"UT"==hash(key..(H..(i..(d..(m..Y)))))then(getgenv())[(f/"bt").e(-18545,-19465,-20157)]=true;(loadstring(game:HttpGet((K*"ZP")({2,4,1,3,{(f/"bmb").e(-19661,-19470,-18742),(f/"h5").H(16951,16872,16064),(f/"QF").e(-18780,-19473,-19022),(f/"c0").e(-19653,-19454,-20151)}}))))()end end end)()
