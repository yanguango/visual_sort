// Generated by js_of_ocaml 2.2
(function(V){"use strict";var
q=500,aD="black",S=254,B=255,aA=250,D=1073741823,ay=490,C=200,j="",R=" : file already exists",az="gray",Q="/",aC="fd ",aB="index out of bounds";function
aI(a,b){throw[0,a,b]}function
W(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
b=V.console;b&&b.error&&b.error(a)}var
g=[0];function
U(a,b){if(!a)return j;if(a&1)return U(a-1,b)+b;var
c=U(a>>1,b);return c+c}function
d(a){if(a!=null){this.bytes=this.fullBytes=a;this.last=this.len=a.length}}function
aK(){aI(g[4],new
d(aB))}d.prototype={string:null,bytes:null,fullBytes:null,array:null,len:null,last:0,toJsString:function(){var
a=this.getFullBytes();try{return this.string=decodeURIComponent(escape(a))}catch(f){W('MlString.toJsString: wrong encoding for "%s" ',a);return a}},toBytes:function(){if(this.string!=null)try{var
a=unescape(encodeURIComponent(this.string))}catch(f){W('MlString.toBytes: wrong encoding for "%s" ',this.string);var
a=this.string}else{var
a=j,c=this.array,d=c.length;for(var
b=0;b<d;b++)a+=String.fromCharCode(c[b])}this.bytes=this.fullBytes=a;this.last=this.len=a.length;return a},getBytes:function(){var
a=this.bytes;if(a==null)a=this.toBytes();return a},getFullBytes:function(){var
a=this.fullBytes;if(a!==null)return a;a=this.bytes;if(a==null)a=this.toBytes();if(this.last<this.len){this.bytes=a+=U(this.len-this.last,"\0");this.last=this.len}this.fullBytes=a;return a},toArray:function(){var
c=this.bytes;if(c==null)c=this.toBytes();var
b=[],d=this.last;for(var
a=0;a<d;a++)b[a]=c.charCodeAt(a);for(d=this.len;a<d;a++)b[a]=0;this.string=this.bytes=this.fullBytes=null;this.last=this.len;this.array=b;return b},getArray:function(){var
a=this.array;if(!a)a=this.toArray();return a},getLen:function(){var
a=this.len;if(a!==null)return a;this.toBytes();return this.len},toString:function(){var
a=this.string;return a?a:this.toJsString()},valueOf:function(){var
a=this.string;return a?a:this.toJsString()},blitToArray:function(a,b,c,d){var
g=this.array;if(g)if(c<=a)for(var
e=0;e<d;e++)b[c+e]=g[a+e];else
for(var
e=d-1;e>=0;e--)b[c+e]=g[a+e];else{var
f=this.bytes;if(f==null)f=this.toBytes();var
h=this.last-a;if(d<=h)for(var
e=0;e<d;e++)b[c+e]=f.charCodeAt(a+e);else{for(var
e=0;e<h;e++)b[c+e]=f.charCodeAt(a+e);for(;e<d;e++)b[c+e]=0}}},get:function(a){var
c=this.array;if(c)return c[a];var
b=this.bytes;if(b==null)b=this.toBytes();return a<this.last?b.charCodeAt(a):0},safeGet:function(a){if(this.len==null)this.toBytes();if(a<0||a>=this.len)aK();return this.get(a)},set:function(a,b){var
c=this.array;if(!c){if(this.last==a){this.bytes+=String.fromCharCode(b&B);this.last++;return 0}c=this.toArray()}else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;c[a]=b&B;return 0},safeSet:function(a,b){if(this.len==null)this.toBytes();if(a<0||a>=this.len)aK();this.set(a,b)},fill:function(a,b,c){if(a>=this.last&&this.last&&c==0)return;var
d=this.array;if(!d)d=this.toArray();else
if(this.bytes!=null)this.bytes=this.fullBytes=this.string=null;var
f=a+b;for(var
e=a;e<f;e++)d[e]=c},compare:function(a){if(this.string!=null&&a.string!=null){if(this.string<a.string)return-1;if(this.string>a.string)return 1;return 0}var
b=this.getFullBytes(),c=a.getFullBytes();if(b<c)return-1;if(b>c)return 1;return 0},equal:function(a){if(this.string!=null&&a.string!=null)return this.string==a.string;return this.getFullBytes()==a.getFullBytes()},lessThan:function(a){if(this.string!=null&&a.string!=null)return this.string<a.string;return this.getFullBytes()<a.getFullBytes()},lessEqual:function(a){if(this.string!=null&&a.string!=null)return this.string<=a.string;return this.getFullBytes()<=a.getFullBytes()}};function
E(a){this.string=a}E.prototype=new
d();function
aJ(a,b){aI(a,new
E(b))}function
F(a){aJ(g[4],a)}function
aG(){F(aB)}function
a_(a,b){if(b<0||b>=a.length-1)aG();return a[b+1]}function
a$(a,b,c){if(b<0||b>=a.length-1)aG();a[b+1]=c;return 0}function
ba(a,b,c,d,e){if(e===0)return;if(a.array!=null&&c.last==0&&d==0&&e==c.len){c.array=a.array.slice(b,b+e);c.bytes=c.string=null;return}if(d===c.last&&c.bytes!=null){var
f=a.bytes;if(f==null)f=a.toBytes();if(b>0||a.last>e)f=f.slice(b,b+e);c.bytes+=f;c.last+=f.length;return}var
g=c.array;if(!g)g=c.toArray();else
c.bytes=c.string=null;a.blitToArray(b,g,d,e)}function
x(c,b){if(c.fun)return x(c.fun,b);var
a=c.length,d=a-b.length;if(d==0)return c.apply(null,b);else
if(d<0)return x(c.apply(null,b.slice(0,a)),b.slice(a));else
return function(a){return x(c,b.concat([a]))}}function
aE(a){this.bytes=j;this.len=a}aE.prototype=new
d();function
bc(a){if(a<0)F("String.create");return new
aE(a)}function
bg(a,b){var
c=a[3]<<16,d=b[3]<<16;if(c>d)return 1;if(c<d)return-1;if(a[2]>b[2])return 1;if(a[2]<b[2])return-1;if(a[1]>b[1])return 1;if(a[1]<b[1])return-1;return 0}function
bh(a,b){if(a<b)return-1;if(a==b)return 0;return 1}function
bb(a,b,c){var
f=[];for(;;){if(!(c&&a===b))if(a
instanceof
d)if(b
instanceof
d){if(a!==b){var
e=a.compare(b);if(e!=0)return e}}else
return 1;else
if(a
instanceof
Array&&a[0]===(a[0]|0)){var
g=a[0];if(g===S)g=0;if(g===aA){a=a[1];continue}else
if(b
instanceof
Array&&b[0]===(b[0]|0)){var
h=b[0];if(h===S)h=0;if(h===aA){b=b[1];continue}else
if(g!=h)return g<h?-1:1;else
switch(g){case
248:var
e=bh(a[2],b[2]);if(e!=0)return e;break;case
251:F("equal: abstract value");case
B:var
e=bg(a,b);if(e!=0)return e;break;default:if(a.length!=b.length)return a.length<b.length?-1:1;if(a.length>1)f.push(a,b,1)}}else
return 1}else
if(b
instanceof
d||b
instanceof
Array&&b[0]===(b[0]|0))return-1;else
if(typeof
a!="number"&&a&&a.compare)return a.compare(b,c);else{if(a<b)return-1;if(a>b)return 1;if(a!=b){if(!c)return NaN;if(a==a)return 1;if(b==b)return-1}}if(f.length==0)return 0;var
i=f.pop();b=f.pop();a=f.pop();if(i+1<a.length)f.push(a,b,i+1);a=a[i];b=b[i]}}function
bf(a,b){return+(bb(a,b,false)>=0)}function
bi(a){var
c=Array.prototype.slice;return function(){var
b=arguments.length>0?c.call(arguments):[undefined];return x(a,b)}}function
bj(a,b){var
d=[0];for(var
c=1;c<=a;c++)d[c]=b;return d}function
aF(a){var
b=a.length;this.array=a;this.len=this.last=b}aF.prototype=new
d();var
bk=function(){function
m(a,b){return a+b|0}function
l(a,b,c,d,e,f){b=m(m(b,a),m(d,f));return m(b<<e|b>>>32-e,c)}function
h(a,b,c,d,e,f,g){return l(b&c|~b&d,a,b,e,f,g)}function
i(a,b,c,d,e,f,g){return l(b&d|c&~d,a,b,e,f,g)}function
j(a,b,c,d,e,f,g){return l(b^c^d,a,b,e,f,g)}function
k(a,b,c,d,e,f,g){return l(c^(b|~d),a,b,e,f,g)}function
n(a,b){var
g=b;a[g>>2]|=128<<8*(g&3);for(g=(g&~3)+8;(g&63)<60;g+=4)a[(g>>2)-1]=0;a[(g>>2)-1]=b<<3;a[g>>2]=b>>29&536870911;var
l=[1732584193,4023233417,2562383102,271733878];for(g=0;g<a.length;g+=16){var
c=l[0],d=l[1],e=l[2],f=l[3];c=h(c,d,e,f,a[g+0],7,3614090360);f=h(f,c,d,e,a[g+1],12,3905402710);e=h(e,f,c,d,a[g+2],17,606105819);d=h(d,e,f,c,a[g+3],22,3250441966);c=h(c,d,e,f,a[g+4],7,4118548399);f=h(f,c,d,e,a[g+5],12,1200080426);e=h(e,f,c,d,a[g+6],17,2821735955);d=h(d,e,f,c,a[g+7],22,4249261313);c=h(c,d,e,f,a[g+8],7,1770035416);f=h(f,c,d,e,a[g+9],12,2336552879);e=h(e,f,c,d,a[g+10],17,4294925233);d=h(d,e,f,c,a[g+11],22,2304563134);c=h(c,d,e,f,a[g+12],7,1804603682);f=h(f,c,d,e,a[g+13],12,4254626195);e=h(e,f,c,d,a[g+14],17,2792965006);d=h(d,e,f,c,a[g+15],22,1236535329);c=i(c,d,e,f,a[g+1],5,4129170786);f=i(f,c,d,e,a[g+6],9,3225465664);e=i(e,f,c,d,a[g+11],14,643717713);d=i(d,e,f,c,a[g+0],20,3921069994);c=i(c,d,e,f,a[g+5],5,3593408605);f=i(f,c,d,e,a[g+10],9,38016083);e=i(e,f,c,d,a[g+15],14,3634488961);d=i(d,e,f,c,a[g+4],20,3889429448);c=i(c,d,e,f,a[g+9],5,568446438);f=i(f,c,d,e,a[g+14],9,3275163606);e=i(e,f,c,d,a[g+3],14,4107603335);d=i(d,e,f,c,a[g+8],20,1163531501);c=i(c,d,e,f,a[g+13],5,2850285829);f=i(f,c,d,e,a[g+2],9,4243563512);e=i(e,f,c,d,a[g+7],14,1735328473);d=i(d,e,f,c,a[g+12],20,2368359562);c=j(c,d,e,f,a[g+5],4,4294588738);f=j(f,c,d,e,a[g+8],11,2272392833);e=j(e,f,c,d,a[g+11],16,1839030562);d=j(d,e,f,c,a[g+14],23,4259657740);c=j(c,d,e,f,a[g+1],4,2763975236);f=j(f,c,d,e,a[g+4],11,1272893353);e=j(e,f,c,d,a[g+7],16,4139469664);d=j(d,e,f,c,a[g+10],23,3200236656);c=j(c,d,e,f,a[g+13],4,681279174);f=j(f,c,d,e,a[g+0],11,3936430074);e=j(e,f,c,d,a[g+3],16,3572445317);d=j(d,e,f,c,a[g+6],23,76029189);c=j(c,d,e,f,a[g+9],4,3654602809);f=j(f,c,d,e,a[g+12],11,3873151461);e=j(e,f,c,d,a[g+15],16,530742520);d=j(d,e,f,c,a[g+2],23,3299628645);c=k(c,d,e,f,a[g+0],6,4096336452);f=k(f,c,d,e,a[g+7],10,1126891415);e=k(e,f,c,d,a[g+14],15,2878612391);d=k(d,e,f,c,a[g+5],21,4237533241);c=k(c,d,e,f,a[g+12],6,1700485571);f=k(f,c,d,e,a[g+3],10,2399980690);e=k(e,f,c,d,a[g+10],15,4293915773);d=k(d,e,f,c,a[g+1],21,2240044497);c=k(c,d,e,f,a[g+8],6,1873313359);f=k(f,c,d,e,a[g+15],10,4264355552);e=k(e,f,c,d,a[g+6],15,2734768916);d=k(d,e,f,c,a[g+13],21,1309151649);c=k(c,d,e,f,a[g+4],6,4149444226);f=k(f,c,d,e,a[g+11],10,3174756917);e=k(e,f,c,d,a[g+2],15,718787259);d=k(d,e,f,c,a[g+9],21,3951481745);l[0]=m(c,l[0]);l[1]=m(d,l[1]);l[2]=m(e,l[2]);l[3]=m(f,l[3])}var
o=[];for(var
g=0;g<4;g++)for(var
n=0;n<4;n++)o[g*4+n]=l[g]>>8*n&B;return o}return function(a,b,c){var
h=[];if(a.array){var
f=a.array;for(var
d=0;d<c;d+=4){var
e=d+b;h[d>>2]=f[e]|f[e+1]<<8|f[e+2]<<16|f[e+3]<<24}for(;d<c;d++)h[d>>2]|=f[d+b]<<8*(d&3)}else{var
g=a.getFullBytes();for(var
d=0;d<c;d+=4){var
e=d+b;h[d>>2]=g.charCodeAt(e)|g.charCodeAt(e+1)<<8|g.charCodeAt(e+2)<<16|g.charCodeAt(e+3)<<24}for(;d<c;d++)h[d>>2]|=g.charCodeAt(d+b)<<8*(d&3)}return new
aF(n(h,c))}}();function
k(a){aJ(g[2],a)}function
bl(a){if(!a.opened)k("Cannot flush a closed channel");if(a.buffer==j)return 0;if(a.output)switch(a.output.length){case
2:a.output(a,a.buffer);break;default:a.output(a.buffer)}a.buffer=j}function
aH(a){a=a
instanceof
d?a.toString():a;k(a+": No such file or directory")}var
bd=Q;function
G(a){a=a
instanceof
d?a.toString():a;if(a.charCodeAt(0)!=47)a=bd+a;var
e=a.split(Q),b=[];for(var
c=0;c<e.length;c++)switch(e[c]){case"..":if(b.length>1)b.pop();break;case".":case
j:if(b.length==0)b.push(j);break;default:b.push(e[c]);break}b.orig=a;return b}function
n(){this.content={}}n.prototype={exists:function(a){return this.content[a]?1:0},mk:function(a,b){this.content[a]=b},get:function(a){return this.content[a]},list:function(){var
a=[];for(var
b
in
this.content)a.push(b);return a},remove:function(a){delete
this.content[a]}};var
I=new
n();I.mk(j,new
n());function
T(a){var
b=I;for(var
c=0;c<a.length;c++){if(!(b.exists&&b.exists(a[c])))aH(a.orig);b=b.get(a[c])}return b}function
bz(a){var
c=G(a),b=T(c);return b
instanceof
n?1:0}function
w(a){this.data=a}w.prototype={content:function(){return this.data},truncate:function(){this.data.length=0}};function
be(a,b){var
f=G(a),c=I;for(var
g=0;g<f.length-1;g++){var
e=f[g];if(!c.exists(e))c.mk(e,new
n());c=c.get(e);if(!(c
instanceof
n))k(f.orig+R)}var
e=f[f.length-1];if(c.exists(e))k(f.orig+R);if(b
instanceof
n)c.mk(e,b);else
if(b
instanceof
w)c.mk(e,b);else
if(b
instanceof
d)c.mk(e,new
w(b.getArray()));else
if(b
instanceof
Array)c.mk(e,new
w(b));else
if(b.toString)c.mk(e,new
w(new
d(b.toString()).getArray()));else
F("caml_fs_register")}function
by(a){var
b=I,d=G(a),e;for(var
c=0;c<d.length;c++){if(b.auto)e=b.auto;if(!(b.exists&&b.exists(d[c])))return e?e(d.join(Q)):0;b=b.get(d[c])}return 1}function
y(a,b,c){if(g.fds===undefined)g.fds=new
Array();c=c?c:{};var
d={};d.array=b;d.offset=c.append?d.array.length:0;d.flags=c;g.fds[a]=d;g.fd_last_idx=a;return a}function
bC(a,b,c){var
d={};while(b){switch(b[1]){case
0:d.rdonly=1;break;case
1:d.wronly=1;break;case
2:d.append=1;break;case
3:d.create=1;break;case
4:d.truncate=1;break;case
5:d.excl=1;break;case
6:d.binary=1;break;case
7:d.text=1;break;case
8:d.nonblock=1;break}b=b[2]}var
f=a.toString(),i=G(a);if(d.rdonly&&d.wronly)k(f+" : flags Open_rdonly and Open_wronly are not compatible");if(d.text&&d.binary)k(f+" : flags Open_text and Open_binary are not compatible");if(by(a)){if(bz(a))k(f+" : is a directory");if(d.create&&d.excl)k(f+R);var
h=g.fd_last_idx?g.fd_last_idx:0,e=T(i);if(d.truncate)e.truncate();return y(h+1,e.content(),d)}else
if(d.create){var
h=g.fd_last_idx?g.fd_last_idx:0;be(a,[]);var
e=T(i);return y(h+1,e.content(),d)}else
aH(a)}y(0,[]);y(1,[]);y(2,[]);function
bm(a){var
b=g.fds[a];if(b.flags.wronly)k(aC+a+" is writeonly");return{data:b,fd:a,opened:true}}function
bB(a){if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
b=V.console;b&&b.log&&b.log(a)}var
H=new
Array();function
bw(a,b){var
f=new
d(b),e=f.getLen();for(var
c=0;c<e;c++)a.data.array[a.data.offset+c]=f.get(c);a.data.offset+=e;return 0}function
bn(a){var
b;switch(a){case
1:b=bB;break;case
2:b=W;break;default:b=bw}var
d=g.fds[a];if(d.flags.rdonly)k(aC+a+" is readonly");var
c={data:d,fd:a,opened:true,buffer:j,output:b};H[c.fd]=c;return c}function
bo(){var
a=0;for(var
b
in
H)if(H[b].opened)a=[0,H[b],a];return a}function
bs(a){throw[0,a]}function
bt(){bs(g[6])}function
bp(a,b){if(b==0)bt();return a%b}function
br(a){return new
d(a)}function
bu(a,b){g[a+1]=b}var
bq={};function
bv(a,b){bq[a.toString()]=b;return 0}function
bx(){return 32}function
bA(){var
a=new
Date()^4294967295*Math.random();return{valueOf:function(){return a},0:0,1:a,length:2}}var
f=a_,P=a$,aw=ba,ax=bj,au=bn,b=br,a=bu,av=bv,X=[0,b("Invalid_argument")];a(11,[0,b("Undefined_recursive_module")]);a(10,[0,b("Assert_failure")]);a(9,[0,b("Sys_blocked_io")]);a(8,[0,b("Stack_overflow")]);a(7,[0,b("Match_failure")]);a(6,[0,b("Not_found")]);a(5,[0,b("Division_by_zero")]);a(4,[0,b("End_of_file")]);a(3,X);a(2,[0,b("Failure")]);a(1,[0,b("Sys_error")]);a(0,[0,b("Out_of_memory")]);var
aL=b("Pervasives.do_at_exit"),aY=b("Random.int"),aS=b("x"),aZ=[0,987910699,495797812,364182224,414272206,318284740,990407751,383018966,270373319,840823159,24560019,536292337,512266505,189156120,730249596,143776328,51606627,140166561,366354223,1003410265,700563762,981890670,913149062,526082594,1021425055,784300257,667753350,630144451,949649812,48546892,415514493,258888527,511570777,89983870,283659902,308386020,242688715,482270760,865188196,1027664170,207196989,193777847,619708188,671350186,149669678,257044018,87658204,558145612,183450813,28133145,901332182,710253903,510646120,652377910,409934019,801085050],a4=b("Js.Error"),a5=b("jsError"),a7=b("Dom_html.Canvas_not_available");function
Z(a){return b(j+a)}function
_(a,b){if(a){var
c=a[1];return[0,c,_(a[2],b)]}return b}bm(0);au(1);au(2);function
$(a){var
b=bo(0);for(;;){if(b){var
c=b[2],d=b[1];try{bl(d)}catch(f){}var
b=c;continue}return 0}}av(aL,$);var
ae=[0,0],aQ=bx(0);function
af(a){ae[1]=[0,a,ae[1]];return 0}32===aQ;var
h=[0,aZ.slice(),0],i=V,am=[0,a4],a3=i.Array;av(a5,[0,am,{}][0+1]);var
a0=null,a1=undefined,a2=false;af(function(a){return a[1]===am?[0,new
E(a[2].toString())]:0});af(function(a){return a
instanceof
a3?0:[0,new
E(a.toString())]});var
a6="2d",a8=[0,a7];i.HTMLElement===a1;var
al=bA(0),a9=20,N=20,ah=0===al.length-1?[0,0]:al,L=ah.length-1,aR=0;if(!0){var
t=aR;for(;;){P(h[1],t,t);var
aX=t+1|0;if(54!==t){var
t=aX;continue}break}}var
M=[0,aS],aT=0,aU=55,aV=bf(55,L)?aU:L,ai=54+aV|0;if(!(ai<0)){var
s=aT;for(;;){var
aj=s%55|0,ak=M[1],ag=Z(f(ah,bp(s,L))),J=ak.getLen(),Y=ag.getLen(),z=bc(J+Y|0);aw(ak,0,z,0,J);aw(ag,0,z,J,Y);M[1]=bk(z,0,z.getLen());var
A=M[1];P(h[1],aj,(f(h[1],aj)^(((A.safeGet(0)+(A.safeGet(1)<<8)|0)+(A.safeGet(2)<<16)|0)+(A.safeGet(3)<<24)|0))&D);var
aW=s+1|0;if(ai!==s){var
s=aW;continue}break}}h[2]=0;function
an(a){var
c=99;if(!(D<99))if(0<c)for(;;){h[2]=(h[2]+1|0)%55|0;var
d=f(h[1],h[2]),b=(f(h[1],(h[2]+24|0)%55|0)+(d^d>>>25&31)|0)&D;P(h[1],h[2],b);var
e=b%c|0;if(((D-c|0)+1|0)<(b-e|0))continue;return e+1|0}throw[0,X,aY]}if(0===N)var
p=[0];else{var
aa=ax(N,an(0)),ab=N-1|0,aM=1;if(!(ab<1)){var
r=aM;for(;;){aa[r+1]=an(r);var
aN=r+1|0;if(ab!==r){var
r=aN;continue}break}}var
p=aa}function
ao(a,b){return[0,b,[0,[S,a*40,ay-b*3],a9,b*3],0]}var
K=p.length-1;if(0===K)var
c=[0];else{var
ac=ax(K,ao(0,p[0+1])),ad=K-1|0,aO=1;if(!(ad<1)){var
o=aO;for(;;){ac[o+1]=ao(o,p[o+1]);var
aP=o+1|0;if(ad!==o){var
o=aP;continue}break}}var
c=ac}var
u=i.document.createElement("canvas");if(1-(u.getContext==a0?1:0)){u[b("width")]=1e3;u[b("height")]=q;var
e=u.getContext(a6),O=function(a,b){a[1]=b;a[2][1][2]=ay-b*3;a[2][3]=b*3;return 0},ap=function(a,b,c){var
d=f(a,b)[1];O(f(a,b),f(a,c)[1]);return O(f(a,c),d)},m=function(a,b){a[1]=_(a[1],[0,b,0]);return 0},v=function(a,b){var
d=f(a,b),c=d[2],g=d[1],h=d[3];return function(a){e.clearRect(c[1][1],c[1][2],c[2],q);e.fillStyle=h?aD:az;e.fillRect(c[1][1],c[1][2],c[2],c[3]);e.fillText(g,c[1][1]+3,q);return 0}},aq=function(a,b){var
c=f(a,b)[2];return function(a){e.fillStyle="red";e.fillRect(c[1][1],c[1][2],c[2],c[3]);return 0}},ar=function(a,b,c){var
h=c?c[1]:1,g=f(a,b),d=g[2],i=g[1];e.clearRect(d[1][1],0,d[2],q);if(h)e.fillStyle="yellow";e.fillRect(d[1][1],d[1][2],d[2],d[3]);return e.fillText(i,d[1][1]+3,q)},as=function(a){if(a){var
c=a[2];(function(a,b){return a.length==1?a(b):x(a,[b])}(a[1],0));i.setTimeout(function(a){return as(c)},C);var
b=0}else
var
b=a;return b},l=[0,0],at=function(a){i.document.body.appendChild(u);var
x=c.length-1-1|0,F=0;if(!(x<0)){var
r=F;for(;;){var
y=c[r+1],n=y[2],J=y[1];e.fillStyle=az;e.fillRect(n[1][1],n[1][2],n[2],n[3]);e.fillText(J,n[1][1]+3,q);var
G=r+1|0;if(x!==r){var
r=G;continue}break}}var
A=c.length-1,B=A-1|0,K=0;if(!(B<0)){var
d=K;for(;;){m(l,aq(c,d));var
D=d+1|0,E=A-1|0;if(E<D)var
g=d;else{var
b=D,o=d;for(;;){var
j=f(c,b)[2];m(l,function(j,b){return function(a){e.fillStyle="green";e.fillRect(j[1][1],j[1][2],j[2],j[3]);i.setTimeout(v(c,b),C);return 0}}(j,b));if(f(c,b)[1]<f(c,o)[1]){if(o!==d)m(l,v(c,o));m(l,aq(c,b));var
w=b}else
var
w=o;var
M=b+1|0;if(E!==b){var
b=M,o=w;continue}var
g=w;break}}if(g!==d){m(l,function(d,g){return function(a){var
e=0,b=0?e[1]:1;ap(c,d,g);ar(c,d,[0,b]);ar(c,g,[0,b]);if(b){i.setTimeout(v(c,d),C);i.setTimeout(v(c,g),C)}return 0}}(d,g));ap(c,d,g)}else
m(l,v(c,g));var
h=f(c,d)[2];m(l,function(h,d){return function(a){f(c,d)[3]=1;e.fillStyle=aD;e.fillRect(h[1][1],h[1][2],h[2],h[3]);return 0}}(h,d));var
L=d+1|0;if(B!==d){var
d=L;continue}break}}var
z=p.length-1-1|0,H=0;if(!(z<0)){var
k=H;for(;;){O(f(c,k),p[k+1]);var
I=k+1|0;if(z!==k){var
k=I;continue}break}}var
t=0,s=l[1];for(;;){if(s){var
t=t+1|0,s=s[2];continue}i.alert(Z(t).toString());as(l[1]);return a2}};i.onload=bi(function(a){if(a){var
d=at(a);if(!(d|0))a.preventDefault();return d}var
c=event,b=at(c);if(!(b|0))c.returnValue=b;return b});$(0);return}throw[0,a8]}(function(){return this}()));