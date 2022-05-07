# algebraic surfaces

import math

type 
  vec3* = object
    x* , y* , z* : float32

# yet another lin algebra oper's & func's 
proc `*`*(v:vec3, f:float32) : vec3 {.inline.}= vec3(x:v.x*f, y:v.y*f, z:v.z*f)
proc `/`*(v:vec3, f:float32) : vec3 {.inline.}= vec3(x:v.x/f, y:v.y/f, z:v.z/f)
proc `/=`*(v:var vec3, f:float32){.inline.} = v.x/=f; v.y/=f;  v.z/=f
proc `-`*(a:vec3, b:float32):vec3 {.inline.}= vec3(x:a.x-b, y:a.y-b, z:a.z-b)
proc `-`*(a,b:vec3):vec3 {.inline.}= vec3(x:b.x-a.x, y:b.y-a.y, z:b.z-a.z)
proc hypot*(v:vec3):float32 {.inline.}= (v.x*v.x + v.y*v.y + v.z*v.z).sqrt
proc `**`*(a,b:vec3):vec3 {.inline.} =vec3(x:a.y*b.z-a.z*b.y, y:a.z*b.x-a.x*b.z, z:a.x*b.y-a.y*b.x) # cross prod
proc normalize*(v:vec3):vec3 {.inline.}=v/v.hypot 
proc normal*(v0, v1, v2: vec3): vec3 =
    let n = (v2 - v0) ** (v1 - v0)
    result = if n == vec3(x:0.0, y:0.0, z:0.0): n else: n.normalize()

proc sqr*(x:float32):float32{.inline.}=x*x

# AS funcs

proc Cap*(u,v:float32) : vec3 = 
  vec3( x : 0.5 * cos(u) * sin(2 * v),
        y : 0.5 * sin(u) * sin(2 * v),
        z : 0.5 * (sqr(cos(v)) - sqr(cos(u)) * sqr(sin(v))) )

proc Boy*(u,v:float32) : vec3 = 
  let
    dv = (2 - sqrt(2.0) * sin(3 * u) * sin(2 * v))
    d1 = (cos(u) * sin(2 * v))
    d2 = sqrt(2.0) * sqr(cos(v))
  vec3( x : (d2 * cos(2 * u) + d1) / dv,
        y : (d2 * sin(2 * u) + d1) / dv,
        z : (3 * sqr(cos(v))) / (2 - sqrt(2.0) * sin(3 * u) * sin(2 * v))  )

proc Roman*(r,t:float32) : vec3 = 
  let
    r2 = r * r
    rq = sqrt(1 - r2)
    st = sin(t)
    ct = cos(t)

  vec3( x : r2 * st * ct,
        y : r * st * rq,
        z : r * ct * rq  )

proc SeaShell*(u,v:float32) : vec3 =
  let
    N = 5.6 # number of turns
    H = 3.5 # height
    P = 2 # power
    L = 4 # Controls spike length
    K = 9

  proc W(u:float32):float32 = (u / (2*PI.float32)) ^ P

  vec3(
    x : W(u) * cos(N * u) * (1 + cos(v)),
    y : W(u) * sin(N * u) * (1 + cos(v)),
    z : W(u) * (sin(v) + (sin(v / 2) ^ K) * L.float32) + H * ((u / (2 * PI.float32)) ^ (P + 1))  )

proc TudorRose*(u,v:float32) : vec3 =
  proc R(u,v:float32):float32=cos(v) * cos(v) * max(abs(sin(4 * u)), 0.9 - 0.2 * abs(cos(8 * u)))
  vec3( x : R(u, v) * cos(u) * cos(v),
        y : R(u, v) * sin(u) * cos(v),
        z : R(u, v) * sin(v) * 0.5 )

proc Breather*(u,v:float32) : vec3 = 
  let
    aa = 0.45 # Values from 0.4 to 0.6 produce sensible results
    w1 = 1 - aa * aa
    w = sqrt(w1)

  proc d(u,v:float32):float32 = aa * (((w * cosh(aa * u)) ^ 2) + ((aa * sin(w * v)) ^ 2))

  vec3( x : -u + (2 * w1 * cosh(aa * u) * sinh(aa * u) / d(u, v)),
        y : 2 * w * cosh(aa * u) * (-(w * cos(v) * cos(w * v)) - (sin(v) * sin(w * v))) / d(u, v),
        z : 2 * w * cosh(aa * u) * (-(w * sin(v) * cos(w * v)) + (cos(v) * sin(w * v))) / d(u, v) )

proc Klein*(u,v:float32) : vec3 = 
  let 
    t = 4.5
    tmp = (4 + 2 * cos(u) * cos(t * v) - sin(2 * u) * sin(t * v))

  vec3( x : sin(v) * tmp,
        y : cos(v) * tmp,
        z : 2 * cos(u) * sin(t * v) + sin(2 * u) * cos(t * v) )

proc Klein0*(u,v:float32) : vec3 =
  vec3( x : if 0 <= u and u < PI.float32:  6 * cos(u) * (1 + sin(u)) + 4 * (1 - 0.5f * cos(u)) * cos(u) * cos(v) 
            else:                  6 * cos(u) * (1 + sin(u)) + 4 * (1 - 0.5f * cos(u)) * cos(v + PI.float32),
        y : if 0 <= u and u < PI.float32:  16 * sin(u) + 4 * (1 - 0.5f * cos(u)) * sin(u) * cos(v) 
              else: 16 * sin(u),
        z : 4 * (1 - 0.5f * cos(u)) * sin(v) )

proc Bour*(u,v:float32) : vec3 =
  vec3( x : u * cos(v) - 0.5 * u * u * cos(2 * v),
        y : -u * sin(v) - 0.5 * u * u * sin(2 * v),
        z : 4 / 3 * pow(u, 1.5) * cos(1.5 * v) )

proc Dini*(u,v:float32) : vec3 =
  var psi = 0.3 # aa
  if psi < 0.001: psi = 0.001
  if psi > 0.999: psi = 0.999
  psi = psi * PI.float32
  let
    sinpsi = sin(psi)
    cospsi = cos(psi)
    g = (u - cospsi * v) / sinpsi
    s = exp(g)
    r = (2 * sinpsi) / (s + 1 / s)
    t = r * (s - 1 / s) * 0.5f

  vec3( x : u - t,
        y : r * cos(v),
        z : r * sin(v) )

proc Enneper*(u,v:float32) : vec3 =
  vec3( x : u - u * u * u / 3 + u * v * v,
        y : v - v * v * v / 3 + v * u * u,
        z : u * u - v * v )

proc Scherk*(u,v:float32) : vec3 =

  let
    aa = 0.1
    vv = v + 0.1

  vec3( x : u,
        y : v,
        z : (ln(abs(cos(aa * vv) / cos(aa * u)))) / aa )

proc CanonicalSpiral*(u,v:float32) : vec3 =
  vec3( x : u * v * sin(15 * v),
        y : v,
        z : u * v * cos(15 * v) )

proc BohemianDome*(u,v:float32) : vec3 =
  let 
    A = 0.5
    B = 1.5
    C = 1.0

  vec3( x : A * cos(u),
        y : B * cos(v) + A * sin(u),
        z : C * sin(v) )

proc AstroidalEllipse*(u,v:float32):vec3=
  let 
    A = 1.0
    B = 1.0
    C = 1.0
  vec3( x : pow(A * cos(u) * cos(v), 3),
        y : pow(B * sin(u) * cos(v), 3),
        z : pow(C * sin(v), 3) )

proc Apple*(u,v:float32):vec3=
  let
    R1 = 4.0
    R2 = 3.8

  vec3( x : cos(u) * (R1 + R2 * cos(v)) + pow((v / PI.float32), 100),
        y : sin(u) * (R1 + R2 * cos(v)) + 0.25 * cos(5 * u),
        z : -2.3 * ln(1 - v * 0.3157) + 6 * sin(v) + 2 * cos(v) )

proc Ammonite*(u,v:float32):vec3=
  proc W(u:float32):float32 = pow(u / (2*PI.float32), 2.2)
  let
    N = 5.6 # number of turns
    F = 120.0 # wave frequency
    A = 0.2 # wave amplitude
    
  vec3( x : W(u) * cos(N * u) * (2 + sin(v + cos(F * u) * A)),
        y : W(u) * sin(N * u) * (2 + sin(v + cos(F * u) * A)),
        z : W(u) * cos(v) )

proc PluckerConoid*(u,v:float32):vec3=
  vec3( x : u * v,
        y : u * sqrt(1 - sqr(v)),
        z : 1 - sqr(v) )

proc Cayley*(u,v:float32):vec3=
  vec3( x : u * sin(v) - u * cos(v),
        y : sqr(u) * sin(v) * cos(v),
        z : u ^ 3 * sqr(sin(v)) * cos(v) )

proc UpDownShell*(u,v:float32):vec3=
  vec3( x : u * sin(u) * cos(v),
        y : u * cos(u) * cos(v),
        z : u * sin(v) )

proc ButterFly*(u, v : float32) : vec3 =
  let t1 = (exp(cos(u)) - 2 * cos(4 * u) + (sin(u / 12) ^ 5)) * sin(v)
  vec3( x : sin(u) * t1,
        y : cos(u) * t1,
        z : sin(v) )

proc Rose*(u, v : float32) : vec3 =
  const 
    a = 1.0
    n = 7.0

  vec3( x:a * sin(n * u) * cos(u) * sin(v), 
        y:a * sin(n * u) * sin(u) * sin(v), 
        z:cos(v) / (n * 3) ) 

proc Kuen*(u , v :float32) : vec3 =
  vec3( x : 2 * cosh(v) * (cos(u) + u * sin(u)) / (cosh(v) * cosh(v) + u * u),
        y : 2 * cosh(v) * (-u * cos(u) + sin(u)) / (cosh(v) * cosh(v) + u * u),
        z : v - (2 * sinh(v) * cosh(v)) / (cosh(v) * cosh(v) + u * u)  )


proc Tanaka*(s,t:float32, n:int) : vec3 =
  const tanaka_params = [[0.0, 4, 3, 4, 5, 7, 4], [0.0, 4, 3, 0, 5, 7, 4], [0.0, 3, 4, 8, 5, 5, 2], [14.0, 3, 1, 8, 5, 5, 2]]
  var a, b1, b2, c, d, w, h:float32
    #[  a  center hole size of a torus
        b1 number of cross
        b2 number of cross
        c  distance from the center of rotation
        d  number of torus
        w  gap width
        h  height ]#

  proc set_param(n : int) =
    var np=n %% tanaka_params.len
    a = tanaka_params[np][0]
    b1 = tanaka_params[np][1]
    b2 = tanaka_params[np][2]
    c = tanaka_params[np][3]
    d = tanaka_params[np][4]
    w = tanaka_params[np][5]
    h = tanaka_params[np][6]
    
  proc f(v:float32):float32 = sin(2*sin(sin(sin(v))))

  set_param(n)
  vec3( x : (a - cos(t) + w * sin(b1 * s)) * cos(b2 * s),
        y : (a - cos(t) + w * sin(b1 * s)) * f(b2 * s),
        z : h * (w * sin(b1 * s) + f(t)) + c )

proc Tanaka0*(u,v:float32):vec3 = Tanaka(u,v,0)
proc Tanaka1*(u,v:float32):vec3 = Tanaka(u,v,1)
proc Tanaka2*(u,v:float32):vec3 = Tanaka(u,v,2)
proc Tanaka3*(u,v:float32):vec3 = Tanaka(u,v,3)

const
  as_names* = ["cap","boy", "roman", "sea shell", "tudor rose", "breather",
            "klein bottle", "klein bottle 0", "bour", "dini", "enneper",
            "scherk", "conical spiral", "bohemian dome", "astrodial ellipse",
            "apple", "ammonite", "plucker comoid", "cayley", "up down shell",
            "butterfly", "rose", "kuen", 
            "tanaka-0", "tanaka-1", "tanaka-2", "tanaka-3"]
            
  as_ranges* =[[(0.0'f32, PI.float32), (0.0'f32, PI.float32)],
            [(0.0'f32, PI.float32), (0.0'f32, PI.float32)],
            [(0.0'f32, 1.0'f32), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32), (0.0'f32, PI.float32)],
            [(-20.0'f32, 20.0'f32), (20.0'f32, 80.0'f32)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(-1.0'f32, 1.0'f32), (-1.0'f32, 1.0'f32)],
            [(1.0'f32, 30.0'f32), (1.0'f32, 30.0'f32)],
            [(0.0'f32, 1.0'f32), (-1.0'f32, 1.0'f32)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (-PI.float32, PI.float32)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(-2.0'f32, 2.0'f32), (-1.0'f32, 1.0'f32)],
            [(0.0'f32, 3.0'f32), (0.0'f32, PI.float32*2)],
            [(-10.0'f32, 10.0'f32), (-10.0'f32, 10.0'f32)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(-4.0'f32, 4.0'f32), (-3.75'f32, +3.75'f32)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)],
            [(0.0'f32, PI.float32*2), (0.0'f32, PI.float32*2)]
            ]

  as_funcs* = [ Cap, Boy, Roman, SeaShell, TudorRose, Breather, 
               Klein, Klein0, Bour, Dini, Enneper, 
               Scherk, CanonicalSpiral, BohemianDome, AstroidalEllipse,
               Apple, Ammonite, PluckerConoid, Cayley, UpDownShell, 
               ButterFly, Rose, Kuen,
               Tanaka0, Tanaka1, Tanaka2, Tanaka3 ]

## interface

type
    QuadNormal* = object
        quad : array[4, vec3]
        nrm : vec3
        
proc scale*(v:var seq[QuadNormal])=
  var
    max=v[0].quad[0].x
    min=max

  for qn in v: 
    for j in 0..<4:
      let vv = qn.quad[j]
      max = max(max, max(vv.x,max(vv.y, vv.z)))
      min = min(min, min(vv.x,min(vv.y, vv.z)))

  var dif = (max-min).abs
  if dif!=0:
    for qn in v.mitems:
      for j in 0..<4:
        qn.quad[j] /= dif

# eval parametric func (quad, normal)

proc eval*(algsrf_func : proc (u,v:float32):vec3, resol:int, p0:(float32,float32), p1:(float32,float32)) : seq[QuadNormal] =

  let
    difU = (p0[0]-p0[1]).abs
    difV = (p1[0]-p1[1]).abs
    fromU=p0[0]
    fromV=p1[0]

    dr = 1 / resol
    dt = dr
  
  proc scaleU(x:float32):float32 = x * difU + fromU
  proc scaleV(x:float32):float32 = x * difV + fromV

  for i in 0..resol:
    let idr = i.float32  * dr

    for j in 0..resol:
      let
        jdt = j.float32 * dt
        jdr = jdt
      
      # add quad, normal
      let q = [algsrf_func(scaleU(idr), scaleV(jdr)), algsrf_func(scaleU(idr+dr), scaleV(jdr)), algsrf_func(scaleU(idr+dr), scaleV(jdr+dt)), algsrf_func(scaleU(idr), scaleV(jdr+dt))]
      result.add QuadNormal(
        quad : q, 
        nrm: normal(q[0], q[1], q[2])
      )

proc eval*(n_as, resol:int) : seq[QuadNormal] =
  result = eval(as_funcs[n_as], resol, as_ranges[n_as][0], as_ranges[n_as][1])
  scale(result)
