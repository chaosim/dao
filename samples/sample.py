from oad import *

oad(
  # oad program samples:
  do.
    write(1),
    while_(1).write(2)
    ,
  loop(10).write(1),
  loop(10)(write(1), write(2)),
  loop(10).write(1).write(2),
  loop().write(1), #无限循环
  set(i, 0), #赋值
  loop(100)(label.a, 
            inc(i), 
            if_(i==1).next, 
            write(i), 
            if_(i==5).exit,
            goto.a, #要不要实现它？？？
            ), #跳到下一轮循环，退出循环
  write(1).until(i<3),
  do(write(1), 1).until(i==3),
  
  '''block comment''',  
  "block comment",
  
  let({a:1,
       b:2},
      write(a,b)
      ),
  do(write(a,b)).where({a:1,b:2}),
  
  lambda_(x)(write(1)),
  fun/a(x)(write(1)), #覆盖定义
  fun/a(x)(write(2)), 
  fun//a(x)(write(2)),#扩充定义
  macro/a(x,y)(write(2)),
  macro//a(x,[y],{a:1})(write(2)), #可选参数，关键字参数
  write(a,b).where({a:1, b:2}),
  use(v.a.b.c, v.x.y),  # import name
  
  v.a.b.c, #
  v.a, v.b, v.c, #变量
  use.a.b/(v.a, v.b),
  use.a.b.all,
  for_(x).in_(range(10)).write(a),
  
  on(x).at(1).write(1)
        .at(2).write(2),
  
  on(x)/{1: write(1),
         2: write(2)
        },
  
  loop(10).some.char(x)*10,  
  
  some.char(x)*10,
  any.char(x)*10,
  
  sub(x, 1),    # 成为x-1, 又可以正常地使用运算符了。
  x-1,          # 与上面意思相同
  -char,        # 可以定义成optional(char),漂亮
  +char,        # 可以定义成some(char)
  char[:],      # char重复任意次（包括0次）
  char[:]/',',  # 逗号分隔的列表
  char*5,       # char重复五次
  char[5],      # char重复5次
  char[:5],     # char小于五次
  char[5:]      # char至少五次
)