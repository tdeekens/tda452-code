var js_jquery = $;

function js_click(obj, callback) {
   obj.click(function(evt) {
      A(
         callback,
         [
            [0, "#" + $(this).attr('id')]
         , 0]
      );
   });
}

config = {
   log: true,
   delimiter: "-",
   start: [0, 0],
   alignment: 'horizontal',
   sizes: {
      'aircraftcarrier': 5,
      'battleship': 4,
      'submarine': 3,
      'destroyer': 3,
      'patrolboat': 2
   }
}

state = {
   boatmodel: undefined,
   start: undefined,
   alignment: undefined
};

selectBoat = function(idx) {
   var $this     = $(idx);
   var boatmodel = $this.attr('id');

   state.boatmodel = boatmodel;
   state.start     = config.start;
   state.alignment = config.alignment;

   console.log('State changed:', state);
};

getState = function() {
   return state.boatmodel + "|" + state.start.join("-") + "|" + state.alignment;
}

addBoat = function(idx) {
   if (state.boatmodel === undefined) {
      alert("Please select a boat to position!");
      return false;
   }

   markHorizontal(false);

   var $this      = $(idx)
       , id       = $this.attr('id')
       , position = id.split(config.delimiter);

   state.start = [position[0]--, position[1]--];

   //markHorizontal(true);

   console.log('State changed:', state);
}

markHorizontal = function(book) {
   var boatmodel = state.boatmodel;

   var row  = state.start[0] + 1
       , cell = state.start[1] + 1;

   var length = cell + config.sizes[boatmodel];
   var $cells = $( $('tr').get( row ) ).find('td');

   $cells = $cells.slice(cell, length);

   if (book == true) {
      $cells.addClass('boat');
      $cells.data('model', boatmodel);
   } else {
      $cells.removeClass('boat')
      $cells.data('model', null);
   }
}

markVertically = function(book) {
   var boatmodel = state.boatmodel;

   var $rows     = $('tr')
       , column = state.start[0] + 1;

   var length = column + config.sizes[boatmodel];

   $rows = $rows.slice(column, length);

   if (book == true) {
      $.each($rows, function(i, row) {
         var $row = $(row);
         var $cell = $( $row.find('td').get(column) );

         $cell.addClass('boat').data('model', boatmodel);
      });
   } else {
      $.each($rows, function(i, row) {
         var $row = $(row);
         var $cell = $( $row.find('td').get(column) );

         $cell.removeClass('boat').data('model', null);
      });
   }
}

flipBoat = function() {
   if(state.alignment === 'horizontal') {
      state.alignment = 'vertical';
      markHorizontal(false);
      markVertically(true);
   } else {
      state.alignment = 'horizontal';
      markVertically(false);
      markHorizontal(true);
   }

   console.log('State changed:', state);
};

lockBoat = function() {
   state.boatmodel = undefined;
   state.start     = undefined;
   state.alignment = undefined;
};

startGame = function() {
   var $tds = $('tbody td:not(.shead)');

   $tds.off();
   $tds.removeClass('boat');

   $('#legend').css('visibility', 'hidden');
   $('table').addClass('game-mode');

   $tds.on('click', shoot);
};

shoot = function(idx) {
   var $this = $(idx);

   $this.text('☠');
};

reset = function() {
   state.boatmodel = undefined;
   state.start     = undefined;
   state.alignment = undefined;

   var $tds = $('tbody td:not(.shead)');
   $tds.removeClass();
   $tds.text('');
   $tds.off();
   $tds.on('click', addBoat);
   $('#legend').css('visibility', 'visible');
};
/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(f.apply === undefined) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        return f.arity === 1 ? f(args[0]) : f.apply(null, args);
    } else if(args.length > f.arity) {
        return f.arity === 1 ? A(f(args.shift()), args)
                             : A(f.apply(null, args.splice(0, f.arity)), args);
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function err(str) {
    die(toJSStr(str)[1]);
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isFloatNaN = isDoubleNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

function hs_eqWord64(a, b) {
    return (a[0] == b[0] && a[1] == b[1]);
}

// Word64# representation: (Word, Word)
function hs_wordToWord64(low) {
    return [0, low];
}

function hs_mkWord64(high, low) {
    return [high, low];
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - e.target.offsetLeft, posy - e.target.offsetTop];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}

function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

// Escape all double quotes in a string
function jsUnquote(str) {
    return str.replace(/"/g, '\\"');
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst(arr,elem+1);})]
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

Integer.fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var getBitsUnsigned = function(self, index) {
  var val = getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (getBits(self, i) != getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = getBits(self, i) >>> 16;
    var a0 = getBits(self, i) & 0xFFFF;

    var b1 = getBits(other, i) >>> 16;
    var b0 = getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return Integer.fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (lessThan(self, Integer.TWO_PWR_24_) &&
      lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = getBits(self, i) >>> 16;
      var a0 = getBits(self, i) & 0xFFFF;

      var b1 = getBits(other, j) >>> 16;
      var b0 = getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(greaterThan(self, Integer.ZERO) != greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) & getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) | getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = getBits(self, i) ^ getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (getBits(self, i - arr_delta) << bit_delta) |
               (getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (getBits(self, i + arr_delta) >>> bit_delta) |
               (getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = Integer.fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
txt = '';
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=0,_1=unCStr("Boat can not be added!"),_2=function(_3,_4){return E(_3)[1]==E(_4)[1];},_5=function(_6,_7){return E(_6)[1]!=E(_7)[1];},_8=[0,_2,_5],_9=function(_a){return E(E(_a)[1]);},_b=function(_c,_d,_e){while(1){var _f=E(_d);if(!_f[0]){return E(_e)[0]==0?true:false;}else{var _g=E(_e);if(!_g[0]){return false;}else{if(!A(_9,[_c,_f[1],_g[1]])){return false;}else{_d=_f[2];_e=_g[2];continue;}}}}},_h=function(_i,_j){return [0,E(_i)[1]+E(_j)[1]|0];},_k=[0],_l=[0,1],_m=[0,0],_n=function(_o,_p){var _q=E(_o);if(!_q[0]){var _r=_q[1],_s=E(_p);return _s[0]==0?_r>=_s[1]:I_compareInt(_s[1],_r)<=0;}else{var _t=_q[1],_u=E(_p);return _u[0]==0?I_compareInt(_t,_u[1])>=0:I_compare(_t,_u[1])>=0;}},_v=function(_w,_x){var _y=E(_w);if(!_y[0]){var _z=_y[1],_A=E(_x);return _A[0]==0?_z>_A[1]:I_compareInt(_A[1],_z)<0;}else{var _B=_y[1],_C=E(_x);return _C[0]==0?I_compareInt(_B,_C[1])>0:I_compare(_B,_C[1])>0;}},_D=function(_E,_F){var _G=E(_E);if(!_G[0]){var _H=_G[1],_I=E(_F);return _I[0]==0?_H<_I[1]:I_compareInt(_I[1],_H)>0;}else{var _J=_G[1],_K=E(_F);return _K[0]==0?I_compareInt(_J,_K[1])<0:I_compare(_J,_K[1])<0;}},_L=function(_M,_N){while(1){var _O=E(_M);if(!_O[0]){var _P=_O[1],_Q=E(_N);if(!_Q[0]){var _R=_Q[1],_S=addC(_P,_R);if(!E(_S[2])){return [0,_S[1]];}else{_M=[1,I_fromInt(_P)];_N=[1,I_fromInt(_R)];continue;}}else{_M=[1,I_fromInt(_P)];_N=_Q;continue;}}else{var _T=E(_N);if(!_T[0]){_M=_O;_N=[1,I_fromInt(_T[1])];continue;}else{return [1,I_add(_O[1],_T[1])];}}}},_U=function(_V,_W,_X,_Y,_Z){if(!_n(_Y,_m)){var _10=function(_11){return !_D(_11,_Z)?A(_V,[_11,new T(function(){return _10(_L(_11,_Y));})]):E(_W);};return _10(_X);}else{var _12=function(_13){return !_v(_13,_Z)?A(_V,[_13,new T(function(){return _12(_L(_13,_Y));})]):E(_W);};return _12(_X);}},_14=[0,0],_15=[0,2],_16=[0,3],_17=[0,4],_18=function(_19,_1a,_1b,_1c){if(!E(_1c)){switch(E(_19)){case 0:return _U(function(_1d,_1e){return [1,[0,new T(function(){return _h(_1a,_1d);}),_1b],_1e];},_k,_14,_l,_17);case 1:return _U(function(_1f,_1g){return [1,[0,new T(function(){return _h(_1a,_1f);}),_1b],_1g];},_k,_14,_l,_16);case 2:return _U(function(_1h,_1i){return [1,[0,new T(function(){return _h(_1a,_1h);}),_1b],_1i];},_k,_14,_l,_15);case 3:return _U(function(_1j,_1k){return [1,[0,new T(function(){return _h(_1a,_1j);}),_1b],_1k];},_k,_14,_l,_15);default:return _U(function(_1l,_1m){return [1,[0,new T(function(){return _h(_1a,_1l);}),_1b],_1m];},_k,_14,_l,_l);}}else{switch(E(_19)){case 0:return _U(function(_1n,_1o){return [1,[0,_1a,new T(function(){return _h(_1b,_1n);})],_1o];},_k,_14,_l,_17);case 1:return _U(function(_1p,_1q){return [1,[0,_1a,new T(function(){return _h(_1b,_1p);})],_1q];},_k,_14,_l,_16);case 2:return _U(function(_1r,_1s){return [1,[0,_1a,new T(function(){return _h(_1b,_1r);})],_1s];},_k,_14,_l,_15);case 3:return _U(function(_1t,_1u){return [1,[0,_1a,new T(function(){return _h(_1b,_1t);})],_1u];},_k,_14,_l,_15);default:return _U(function(_1v,_1w){return [1,[0,_1a,new T(function(){return _h(_1b,_1v);})],_1w];},_k,_14,_l,_l);}}},_1x=function(_1y,_1z){var _1A=E(_1y);return _1A[0]==0?E(_1z):[1,_1A[1],new T(function(){return _1x(_1A[2],_1z);})];},_1B=function(_1C){var _1D=E(_1C);if(!_1D[0]){return [0];}else{var _1E=E(_1D[1]),_1F=E(_1E[2]);return _1x(_18(_1E[1],_1F[1],_1F[2],_1E[3]),new T(function(){return _1B(_1D[2]);}));}},_1G=function(_1H,_1I){switch(E(_1H)){case 0:switch(E(_1I)){case 0:return false;case 1:return true;case 2:return true;case 3:return true;default:return true;}break;case 1:return E(_1I)==1?false:true;case 2:return E(_1I)==2?false:true;case 3:return E(_1I)==3?false:true;default:return E(_1I)==4?false:true;}},_1J=function(_1K,_1L){switch(E(_1K)){case 0:switch(E(_1L)){case 0:return true;case 1:return false;case 2:return false;case 3:return false;default:return false;}break;case 1:return E(_1L)==1?true:false;case 2:return E(_1L)==2?true:false;case 3:return E(_1L)==3?true:false;default:return E(_1L)==4?true:false;}},_1M=[0,_1J,_1G],_1N=function(_1O,_1P){while(1){var _1Q=E(_1O);if(!_1Q[0]){return E(_1P);}else{_1O=_1Q[2];var _1R=_1P+1|0;_1P=_1R;continue;}}},_1S=0,_1T=1,_1U=function(_1V,_1W,_1X){while(1){var _1Y=E(_1X);if(!_1Y[0]){return false;}else{if(!A(_9,[_1V,_1W,_1Y[1]])){_1X=_1Y[2];continue;}else{return true;}}}},_1Z=function(_20,_21){var _22=function(_23,_24){while(1){var _25=(function(_26,_27){var _28=E(_27);if(!_28[0]){return [0];}else{var _29=_28[2];if(!A(_20,[_28[1]])){var _2a=_26+1|0;_24=_29;_23=_2a;return null;}else{return [1,[0,_26],new T(function(){return _22(_26+1|0,_29);})];}}})(_23,_24);if(_25!=null){return _25;}}};return _22(0,_21);},_2b=unCStr("base"),_2c=unCStr("Control.Exception.Base"),_2d=unCStr("PatternMatchFail"),_2e=[0,18445595,52003073,_2b,_2c,_2d],_2f=[0,18445595,52003073,_2e,_k],_2g=function(_2h){return E(_2f);},_2i=function(_2j){return E(E(_2j)[1]);},_2k=unCStr("Maybe.fromJust: Nothing"),_2l=new T(function(){return err(_2k);}),_2m=function(_2n,_2o,_2p){var _2q=new T(function(){var _2r=A(_2n,[_2p]),_2s=A(_2o,[new T(function(){var _2t=E(_2q);return _2t[0]==0?E(_2l):E(_2t[1]);})]),_2u=hs_eqWord64(_2r[1],_2s[1]);if(!E(_2u)){return [0];}else{var _2v=hs_eqWord64(_2r[2],_2s[2]);return E(_2v)==0?[0]:[1,_2p];}});return E(_2q);},_2w=function(_2x){var _2y=E(_2x);return _2m(_2i(_2y[1]),_2g,_2y[2]);},_2z=function(_2A){return E(E(_2A)[1]);},_2B=function(_2C,_2D){return _1x(E(_2C)[1],_2D);},_2E=[0,44],_2F=[0,93],_2G=[0,91],_2H=function(_2I,_2J,_2K){var _2L=E(_2J);return _2L[0]==0?unAppCStr("[]",_2K):[1,_2G,new T(function(){return A(_2I,[_2L[1],new T(function(){var _2M=function(_2N){var _2O=E(_2N);return _2O[0]==0?E([1,_2F,_2K]):[1,_2E,new T(function(){return A(_2I,[_2O[1],new T(function(){return _2M(_2O[2]);})]);})];};return _2M(_2L[2]);})]);})];},_2P=function(_2Q,_2R){return _2H(_2B,_2Q,_2R);},_2S=function(_2T,_2U,_2V){return _1x(E(_2U)[1],_2V);},_2W=[0,_2S,_2z,_2P],_2X=[0,_2g,_2W,_2Y,_2w],_2Y=function(_2Z){return [0,_2X,_2Z];},_30=unCStr("Non-exhaustive patterns in"),_31=function(_32,_33){return die(new T(function(){return A(_33,[_32]);}));},_34=function(_35,_36){var _37=E(_36);if(!_37[0]){return [0,_k,_k];}else{var _38=_37[1];if(!A(_35,[_38])){return [0,_k,_37];}else{var _39=new T(function(){var _3a=_34(_35,_37[2]);return [0,_3a[1],_3a[2]];});return [0,[1,_38,new T(function(){return E(E(_39)[1]);})],new T(function(){return E(E(_39)[2]);})];}}},_3b=[0,32],_3c=[0,10],_3d=[1,_3c,_k],_3e=function(_3f){return E(E(_3f)[1])==124?false:true;},_3g=function(_3h,_3i){var _3j=_34(_3e,unCStr(_3h)),_3k=_3j[1],_3l=function(_3m,_3n){return _1x(_3m,new T(function(){return unAppCStr(": ",new T(function(){return _1x(_3i,new T(function(){return _1x(_3n,_3d);}));}));}));},_3o=E(_3j[2]);return _3o[0]==0?_3l(_3k,_k):E(E(_3o[1])[1])==124?_3l(_3k,[1,_3b,_3o[2]]):_3l(_3k,_k);},_3p=function(_3q){return _31([0,new T(function(){return _3g(_3q,_30);})],_2Y);},_3r=new T(function(){return _3p("Battleship.hs:(71,1)-(77,46)|function spaceLeftForModel");}),_3s=function(_3t){return E(E(_3t)[1]);},_3u=function(_3v){var _3w=E(_3v);return _3w[0]==0?[0]:[1,new T(function(){return _3s(_3w[1]);}),new T(function(){return _3u(_3w[2]);})];},_3x=function(_3y,_3z){var _3A=_3u(_3y),_3B=new T(function(){var _3C=E(_3z);if(_3C==1){return !_1U(_1M,_1T,_3A)?true:false;}else{var _3D=new T(function(){return _1N(_1Z(function(_3E){switch(E(_3C)){case 0:switch(E(_3E)){case 0:return true;case 1:return false;case 2:return false;case 3:return false;default:return false;}break;case 2:return E(_3E)==2?true:false;case 3:return E(_3E)==3?true:false;default:return E(_3E)==4?true:false;}},_3A),0)<2;});switch(E(_3C)){case 3:return E(_3D);case 4:return E(_3D);default:return E(_3r);}}});switch(E(_3z)){case 0:return !_1U(_1M,_1S,_3A)?true:false;case 1:return E(_3B);case 2:return E(_3B);case 3:return E(_3B);default:return E(_3B);}},_3F=function(_3G,_3H){while(1){var _3I=E(_3H);if(!_3I[0]){return false;}else{if(!A(_3G,[_3I[1]])){_3H=_3I[2];continue;}else{return true;}}}},_3J=function(_3K,_3L){if(_3K<=_3L){var _3M=function(_3N){return [1,[0,_3N],new T(function(){return _3N!=_3L?_3M(_3N+1|0):[0];})];};return _3M(_3K);}else{return [0];}},_3O=new T(function(){return _3J(0,9);}),_3P=function(_3Q,_3R,_3S,_3T,_3U,_3V){return !A(_3Q,[_3S,_3U])?true:!A(_9,[_3R,_3T,_3V])?true:false;},_3W=function(_3X,_3Y,_3Z,_40){var _41=E(_3Z),_42=E(_40);return _3P(E(_3X)[1],_3Y,_41[1],_41[2],_42[1],_42[2]);},_43=function(_44,_45,_46,_47,_48,_49){return !A(_44,[_46,_48])?false:A(_9,[_45,_47,_49]);},_4a=function(_4b,_4c,_4d,_4e){var _4f=E(_4d),_4g=E(_4e);return _43(E(_4b)[1],_4c,_4f[1],_4f[2],_4g[1],_4g[2]);},_4h=function(_4i,_4j){return [0,function(_4k,_4l){return _4a(_4i,_4j,_4k,_4l);},function(_4k,_4l){return _3W(_4i,_4j,_4k,_4l);}];},_4m=new T(function(){return _4h(_8,_8);}),_4n=new T(function(){return _b(_4m,_k,_k);}),_4o=function(_4p,_4q){var _4r=_1B(_4p);if(!_4r[0]){if(!E(_4n)){return false;}else{var _4s=E(_4q),_4t=E(_4s[2]);return !_1U(_8,_4t[1],_3O)?false:!_1U(_8,_4t[2],_3O)?false:_3x(_4p,_4s[1]);}}else{var _4u=E(_4q),_4v=_4u[1],_4w=E(_4u[2]),_4x=_4w[1],_4y=_4w[2],_4z=_18(_4v,_4x,_4y,_4u[3]);if(!_4z[0]){return !E(_4n)?false:!_1U(_8,_4x,_3O)?false:!_1U(_8,_4y,_3O)?false:_3x(_4p,_4v);}else{var _4A=function(_4B){while(1){var _4C=(function(_4D){var _4E=E(_4D);if(!_4E[0]){return [0];}else{var _4F=_4E[1],_4G=_4E[2];if(!_3F(function(_4H){var _4I=E(_4F),_4J=E(_4H);return E(_4I[1])[1]!=E(_4J[1])[1]?false:_2(_4I[2],_4J[2]);},_4z)){_4B=_4G;return null;}else{return [1,_4F,new T(function(){return _4A(_4G);})];}}})(_4B);if(_4C!=null){return _4C;}}};return !_b(_4m,(function(_4K,_4L){return !_3F(function(_4M){var _4N=E(_4K),_4O=E(_4M);return E(_4N[1])[1]!=E(_4O[1])[1]?false:_2(_4N[2],_4O[2]);},_4z)?_4A(_4L):[1,_4K,new T(function(){return _4A(_4L);})];})(_4r[1],_4r[2]),_k)?false:!_1U(_8,_4x,_3O)?false:!_1U(_8,_4y,_3O)?false:_3x(_4p,_4v);}}},_4P=unCStr("horizontal"),_4Q=unCStr("Prelude.(!!): index too large\n"),_4R=new T(function(){return err(_4Q);}),_4S=function(_4T,_4U){while(1){var _4V=E(_4T);if(!_4V[0]){return E(_4R);}else{var _4W=E(_4U);if(!_4W){return E(_4V[1]);}else{_4T=_4V[2];_4U=_4W-1|0;continue;}}}},_4X=new T(function(){var _4Y=getState();return fromJSStr(_4Y);}),_4Z=unCStr(": empty list"),_50=unCStr("Prelude."),_51=function(_52){return err(_1x(_50,new T(function(){return _1x(_52,_4Z);})));},_53=unCStr("head"),_54=new T(function(){return _51(_53);}),_55=unCStr("tail"),_56=new T(function(){return _51(_55);}),_57=function(_58,_59){var _5a=E(_58);if(!_5a[0]){return [0];}else{var _5b=E(_5a[1]),_5c=new T(function(){return _57(_5a[2],_59);});return _5b[1]!=_59?[1,[1,_5b,new T(function(){var _5d=E(_5c);return _5d[0]==0?E(_54):E(_5d[1]);})],new T(function(){var _5e=E(_5c);return _5e[0]==0?E(_56):E(_5e[2]);})]:[1,_k,_5c];}},_5f=new T(function(){return _57(_4X,124);}),_5g=new T(function(){return _4S(_5f,0);}),_5h=function(_5i,_5j){while(1){var _5k=E(_5i);if(!_5k[0]){return E(_5j)[0]==0?true:false;}else{var _5l=E(_5j);if(!_5l[0]){return false;}else{if(E(_5k[1])[1]!=E(_5l[1])[1]){return false;}else{_5i=_5k[2];_5j=_5l[2];continue;}}}}},_5m=new T(function(){return !_5h(_5g,_4P)?0:1;}),_5n=unCStr("Prelude.read: ambiguous parse"),_5o=new T(function(){return err(_5n);}),_5p=unCStr("Prelude.read: no parse"),_5q=new T(function(){return err(_5p);}),_5r=new T(function(){return _3p("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus");}),_5s=function(_5t,_5u){while(1){var _5v=(function(_5w,_5x){var _5y=E(_5w);switch(_5y[0]){case 0:var _5z=E(_5x);if(!_5z[0]){return [0];}else{_5t=A(_5y[1],[_5z[1]]);_5u=_5z[2];return null;}break;case 1:var _5A=A(_5y[1],[_5x]),_5B=_5x;_5t=_5A;_5u=_5B;return null;case 2:return [0];case 3:return [1,[0,_5y[1],_5x],new T(function(){return _5s(_5y[2],_5x);})];default:return E(_5y[1]);}})(_5t,_5u);if(_5v!=null){return _5v;}}},_5C=function(_5D,_5E){var _5F=new T(function(){var _5G=E(_5E);if(_5G[0]==3){return [3,_5G[1],new T(function(){return _5C(_5D,_5G[2]);})];}else{var _5H=E(_5D);if(_5H[0]==2){return E(_5G);}else{var _5I=E(_5G);if(_5I[0]==2){return E(_5H);}else{var _5J=new T(function(){var _5K=E(_5I);if(_5K[0]==4){return [1,function(_5L){return [4,new T(function(){return _1x(_5s(_5H,_5L),_5K[1]);})];}];}else{var _5M=E(_5H);if(_5M[0]==1){var _5N=_5M[1],_5O=E(_5K);return _5O[0]==0?[1,function(_5P){return _5C(A(_5N,[_5P]),_5O);}]:[1,function(_5Q){return _5C(A(_5N,[_5Q]),new T(function(){return A(_5O[1],[_5Q]);}));}];}else{var _5R=E(_5K);return _5R[0]==0?E(_5r):[1,function(_5S){return _5C(_5M,new T(function(){return A(_5R[1],[_5S]);}));}];}}}),_5T=E(_5H);switch(_5T[0]){case 1:var _5U=E(_5I);return _5U[0]==4?[1,function(_5V){return [4,new T(function(){return _1x(_5s(A(_5T[1],[_5V]),_5V),_5U[1]);})];}]:E(_5J);case 4:var _5W=_5T[1],_5X=E(_5I);switch(_5X[0]){case 0:return [1,function(_5Y){return [4,new T(function(){return _1x(_5W,new T(function(){return _5s(_5X,_5Y);}));})];}];case 1:return [1,function(_5Z){return [4,new T(function(){return _1x(_5W,new T(function(){return _5s(A(_5X[1],[_5Z]),_5Z);}));})];}];default:return [4,new T(function(){return _1x(_5W,_5X[1]);})];}break;default:return E(_5J);}}}}}),_60=E(_5D);switch(_60[0]){case 0:var _61=E(_5E);return _61[0]==0?[0,function(_62){return _5C(A(_60[1],[_62]),new T(function(){return A(_61[1],[_62]);}));}]:E(_5F);case 3:return [3,_60[1],new T(function(){return _5C(_60[2],_5E);})];default:return E(_5F);}},_63=function(_64,_65){return E(_64)[1]!=E(_65)[1];},_66=function(_67,_68){return E(_67)[1]==E(_68)[1];},_69=[0,_66,_63],_6a=function(_6b,_6c,_6d){return !_b(_6b,_6c,_6d)?true:false;},_6e=function(_6f){return [0,function(_4k,_4l){return _b(_6f,_4k,_4l);},function(_4k,_4l){return _6a(_6f,_4k,_4l);}];},_6g=new T(function(){return _6e(_69);}),_6h=function(_6i,_6j){var _6k=E(_6i);switch(_6k[0]){case 0:return [0,function(_6l){return _6h(A(_6k[1],[_6l]),_6j);}];case 1:return [1,function(_6m){return _6h(A(_6k[1],[_6m]),_6j);}];case 2:return [2];case 3:return _5C(A(_6j,[_6k[1]]),new T(function(){return _6h(_6k[2],_6j);}));default:var _6n=function(_6o){var _6p=E(_6o);if(!_6p[0]){return [0];}else{var _6q=E(_6p[1]);return _1x(_5s(A(_6j,[_6q[1]]),_6q[2]),new T(function(){return _6n(_6p[2]);}));}},_6r=_6n(_6k[1]);return _6r[0]==0?[2]:[4,_6r];}},_6s=[2],_6t=function(_6u){return [3,_6u,_6s];},_6v=function(_6w,_6x){var _6y=E(_6w);return _6y==0?A(_6x,[_0]):[0,function(_6z){return _6v(_6y-1|0,_6x);}];},_6A=function(_6B,_6C,_6D){return [1,function(_6E){return A(function(_6F,_6G,_6H){while(1){var _6I=(function(_6J,_6K,_6L){var _6M=E(_6J);switch(_6M[0]){case 0:var _6N=E(_6K);if(!_6N[0]){return E(_6C);}else{_6F=A(_6M[1],[_6N[1]]);_6G=_6N[2];var _6O=_6L+1|0;_6H=_6O;return null;}break;case 1:var _6P=A(_6M[1],[_6K]),_6Q=_6K,_6O=_6L;_6F=_6P;_6G=_6Q;_6H=_6O;return null;case 2:return E(_6C);case 3:return function(_6R){return _6v(_6L,function(_6S){return _6h(_6M,_6R);});};default:return function(_6T){return _6h(_6M,_6T);};}})(_6F,_6G,_6H);if(_6I!=null){return _6I;}}},[new T(function(){return A(_6B,[_6t]);}),_6E,0,_6D]);}];},_6U=[6],_6V=function(_6W){return E(_6W);},_6X=unCStr("valDig: Bad base"),_6Y=new T(function(){return err(_6X);}),_6Z=function(_70,_71){var _72=function(_73,_74){var _75=E(_73);if(!_75[0]){return function(_76){return A(_76,[new T(function(){return A(_74,[_k]);})]);};}else{var _77=E(_75[1])[1],_78=function(_79){return function(_7a){return [0,function(_7b){return A(_72(_75[2],function(_7c){return A(_74,[[1,_79,_7c]]);}),[_7a]);}];};};switch(E(E(_70)[1])){case 8:return 48>_77?function(_7d){return A(_7d,[new T(function(){return A(_74,[_k]);})]);}:_77>55?function(_7e){return A(_7e,[new T(function(){return A(_74,[_k]);})]);}:_78([0,_77-48|0]);case 10:return 48>_77?function(_7f){return A(_7f,[new T(function(){return A(_74,[_k]);})]);}:_77>57?function(_7g){return A(_7g,[new T(function(){return A(_74,[_k]);})]);}:_78([0,_77-48|0]);case 16:var _7h=new T(function(){return 97>_77?65>_77?[0]:_77>70?[0]:[1,[0,(_77-65|0)+10|0]]:_77>102?65>_77?[0]:_77>70?[0]:[1,[0,(_77-65|0)+10|0]]:[1,[0,(_77-97|0)+10|0]];});if(48>_77){var _7i=E(_7h);return _7i[0]==0?function(_7j){return A(_7j,[new T(function(){return A(_74,[_k]);})]);}:_78(_7i[1]);}else{if(_77>57){var _7k=E(_7h);return _7k[0]==0?function(_7l){return A(_7l,[new T(function(){return A(_74,[_k]);})]);}:_78(_7k[1]);}else{return _78([0,_77-48|0]);}}break;default:return E(_6Y);}}};return [1,function(_7m){return A(_72,[_7m,_6V,function(_7n){var _7o=E(_7n);return _7o[0]==0?[2]:A(_71,[_7o]);}]);}];},_7p=[0,10],_7q=[0,1],_7r=[0,2147483647],_7s=new T(function(){return _L(_7r,_7q);}),_7t=function(_7u){var _7v=E(_7u);if(!_7v[0]){var _7w=E(_7v[1]);return _7w==(-2147483648)?E(_7s):[0, -_7w];}else{return [1,I_negate(_7v[1])];}},_7x=[0,10],_7y=[0,0],_7z=function(_7A,_7B){while(1){var _7C=E(_7A);if(!_7C[0]){var _7D=_7C[1],_7E=E(_7B);if(!_7E[0]){var _7F=_7E[1];if(!(imul(_7D,_7F)|0)){return [0,imul(_7D,_7F)|0];}else{_7A=[1,I_fromInt(_7D)];_7B=[1,I_fromInt(_7F)];continue;}}else{_7A=[1,I_fromInt(_7D)];_7B=_7E;continue;}}else{var _7G=E(_7B);if(!_7G[0]){_7A=_7C;_7B=[1,I_fromInt(_7G[1])];continue;}else{return [1,I_mul(_7C[1],_7G[1])];}}}},_7H=function(_7I,_7J,_7K){while(1){var _7L=E(_7K);if(!_7L[0]){return E(_7J);}else{var _7M=_L(_7z(_7J,_7I),_7L[1]);_7K=_7L[2];_7J=_7M;continue;}}},_7N=function(_7O){var _7P=new T(function(){return _5C(_5C([0,function(_7Q){return E(E(_7Q)[1])==45?_6Z(_7p,function(_7R){return A(_7O,[[1,new T(function(){return _7t(_7H(_7x,_7y,_7R));})]]);}):[2];}],[0,function(_7S){return E(E(_7S)[1])==43?_6Z(_7p,function(_7T){return A(_7O,[[1,new T(function(){return _7H(_7x,_7y,_7T);})]]);}):[2];}]),new T(function(){return _6Z(_7p,function(_7U){return A(_7O,[[1,new T(function(){return _7H(_7x,_7y,_7U);})]]);});}));});return _5C([0,function(_7V){return E(E(_7V)[1])==101?E(_7P):[2];}],[0,function(_7W){return E(E(_7W)[1])==69?E(_7P):[2];}]);},_7X=[0],_7Y=function(_7Z){return A(_7Z,[_7X]);},_80=function(_81){return A(_81,[_7X]);},_82=function(_83){return [0,function(_84){return E(E(_84)[1])==46?_6Z(_7p,function(_85){return A(_83,[[1,_85]]);}):[2];}];},_86=function(_87){return _6Z(_7p,function(_88){return _6A(_82,_7Y,function(_89){return _6A(_7N,_80,function(_8a){return A(_87,[[5,[1,_88,_89,_8a]]]);});});});},_8b=unCStr("!@#$%&*+./<=>?\\^|:-~"),_8c=function(_8d){return _1U(_69,_8d,_8b);},_8e=[0,8],_8f=[0,16],_8g=function(_8h){return [0,function(_8i){return E(E(_8i)[1])==48?E([0,function(_8j){switch(E(E(_8j)[1])){case 79:return _6Z(_8e,function(_8k){return A(_8h,[[5,[0,_8e,_8k]]]);});case 88:return _6Z(_8f,function(_8l){return A(_8h,[[5,[0,_8f,_8l]]]);});case 111:return _6Z(_8e,function(_8m){return A(_8h,[[5,[0,_8e,_8m]]]);});case 120:return _6Z(_8f,function(_8n){return A(_8h,[[5,[0,_8f,_8n]]]);});default:return [2];}}]):[2];}];},_8o=false,_8p=true,_8q=function(_8r){return [0,function(_8s){switch(E(E(_8s)[1])){case 79:return A(_8r,[_8e]);case 88:return A(_8r,[_8f]);case 111:return A(_8r,[_8e]);case 120:return A(_8r,[_8f]);default:return [2];}}];},_8t=function(_8u){return A(_8u,[_7p]);},_8v=function(_8w,_8x){var _8y=jsShowI(_8w);return _1x(fromJSStr(_8y),_8x);},_8z=[0,41],_8A=[0,40],_8B=function(_8C,_8D,_8E){return _8D>=0?_8v(_8D,_8E):_8C<=6?_8v(_8D,_8E):[1,_8A,new T(function(){var _8F=jsShowI(_8D);return _1x(fromJSStr(_8F),[1,_8z,_8E]);})];},_8G=function(_8H){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _8B(9,_8H,_k);})));},_8I=function(_8J){var _8K=E(_8J);return _8K[0]==0?E(_8K[1]):I_toInt(_8K[1]);},_8L=function(_8M,_8N){var _8O=E(_8M);if(!_8O[0]){var _8P=_8O[1],_8Q=E(_8N);return _8Q[0]==0?_8P<=_8Q[1]:I_compareInt(_8Q[1],_8P)>=0;}else{var _8R=_8O[1],_8S=E(_8N);return _8S[0]==0?I_compareInt(_8R,_8S[1])<=0:I_compare(_8R,_8S[1])<=0;}},_8T=function(_8U){return [2];},_8V=function(_8W){var _8X=E(_8W);if(!_8X[0]){return E(_8T);}else{var _8Y=_8X[1],_8Z=E(_8X[2]);return _8Z[0]==0?E(_8Y):function(_90){return _5C(A(_8Y,[_90]),new T(function(){return A(_8V(_8Z),[_90]);}));};}},_91=unCStr("NUL"),_92=function(_93){return [2];},_94=function(_95){return _92(_95);},_96=function(_97,_98){var _99=function(_9a,_9b){var _9c=E(_9a);if(!_9c[0]){return function(_9d){return A(_9d,[_97]);};}else{var _9e=E(_9b);return _9e[0]==0?E(_92):E(_9c[1])[1]!=E(_9e[1])[1]?E(_94):function(_9f){return [0,function(_9g){return A(_99(_9c[2],_9e[2]),[_9f]);}];};}};return [1,function(_9h){return A(_99,[_97,_9h,_98]);}];},_9i=[0,0],_9j=function(_9k){return _96(_91,function(_9l){return A(_9k,[_9i]);});},_9m=unCStr("STX"),_9n=[0,2],_9o=function(_9p){return _96(_9m,function(_9q){return A(_9p,[_9n]);});},_9r=unCStr("ETX"),_9s=[0,3],_9t=function(_9u){return _96(_9r,function(_9v){return A(_9u,[_9s]);});},_9w=unCStr("EOT"),_9x=[0,4],_9y=function(_9z){return _96(_9w,function(_9A){return A(_9z,[_9x]);});},_9B=unCStr("ENQ"),_9C=[0,5],_9D=function(_9E){return _96(_9B,function(_9F){return A(_9E,[_9C]);});},_9G=unCStr("ACK"),_9H=[0,6],_9I=function(_9J){return _96(_9G,function(_9K){return A(_9J,[_9H]);});},_9L=unCStr("BEL"),_9M=[0,7],_9N=function(_9O){return _96(_9L,function(_9P){return A(_9O,[_9M]);});},_9Q=unCStr("BS"),_9R=[0,8],_9S=function(_9T){return _96(_9Q,function(_9U){return A(_9T,[_9R]);});},_9V=unCStr("HT"),_9W=[0,9],_9X=function(_9Y){return _96(_9V,function(_9Z){return A(_9Y,[_9W]);});},_a0=unCStr("LF"),_a1=[0,10],_a2=function(_a3){return _96(_a0,function(_a4){return A(_a3,[_a1]);});},_a5=unCStr("VT"),_a6=[0,11],_a7=function(_a8){return _96(_a5,function(_a9){return A(_a8,[_a6]);});},_aa=unCStr("FF"),_ab=[0,12],_ac=function(_ad){return _96(_aa,function(_ae){return A(_ad,[_ab]);});},_af=unCStr("CR"),_ag=[0,13],_ah=function(_ai){return _96(_af,function(_aj){return A(_ai,[_ag]);});},_ak=unCStr("SI"),_al=[0,15],_am=function(_an){return _96(_ak,function(_ao){return A(_an,[_al]);});},_ap=unCStr("DLE"),_aq=[0,16],_ar=function(_as){return _96(_ap,function(_at){return A(_as,[_aq]);});},_au=unCStr("DC1"),_av=[0,17],_aw=function(_ax){return _96(_au,function(_ay){return A(_ax,[_av]);});},_az=unCStr("DC2"),_aA=[0,18],_aB=function(_aC){return _96(_az,function(_aD){return A(_aC,[_aA]);});},_aE=unCStr("DC3"),_aF=[0,19],_aG=function(_aH){return _96(_aE,function(_aI){return A(_aH,[_aF]);});},_aJ=unCStr("DC4"),_aK=[0,20],_aL=function(_aM){return _96(_aJ,function(_aN){return A(_aM,[_aK]);});},_aO=unCStr("NAK"),_aP=[0,21],_aQ=function(_aR){return _96(_aO,function(_aS){return A(_aR,[_aP]);});},_aT=unCStr("SYN"),_aU=[0,22],_aV=function(_aW){return _96(_aT,function(_aX){return A(_aW,[_aU]);});},_aY=unCStr("ETB"),_aZ=[0,23],_b0=function(_b1){return _96(_aY,function(_b2){return A(_b1,[_aZ]);});},_b3=unCStr("CAN"),_b4=[0,24],_b5=function(_b6){return _96(_b3,function(_b7){return A(_b6,[_b4]);});},_b8=unCStr("EM"),_b9=[0,25],_ba=function(_bb){return _96(_b8,function(_bc){return A(_bb,[_b9]);});},_bd=unCStr("SUB"),_be=[0,26],_bf=function(_bg){return _96(_bd,function(_bh){return A(_bg,[_be]);});},_bi=unCStr("ESC"),_bj=[0,27],_bk=function(_bl){return _96(_bi,function(_bm){return A(_bl,[_bj]);});},_bn=unCStr("FS"),_bo=[0,28],_bp=function(_bq){return _96(_bn,function(_br){return A(_bq,[_bo]);});},_bs=unCStr("GS"),_bt=[0,29],_bu=function(_bv){return _96(_bs,function(_bw){return A(_bv,[_bt]);});},_bx=unCStr("RS"),_by=[0,30],_bz=function(_bA){return _96(_bx,function(_bB){return A(_bA,[_by]);});},_bC=unCStr("US"),_bD=[0,31],_bE=function(_bF){return _96(_bC,function(_bG){return A(_bF,[_bD]);});},_bH=unCStr("SP"),_bI=[0,32],_bJ=function(_bK){return _96(_bH,function(_bL){return A(_bK,[_bI]);});},_bM=unCStr("DEL"),_bN=[0,127],_bO=function(_bP){return _96(_bM,function(_bQ){return A(_bP,[_bN]);});},_bR=[1,_bO,_k],_bS=[1,_bJ,_bR],_bT=[1,_bE,_bS],_bU=[1,_bz,_bT],_bV=[1,_bu,_bU],_bW=[1,_bp,_bV],_bX=[1,_bk,_bW],_bY=[1,_bf,_bX],_bZ=[1,_ba,_bY],_c0=[1,_b5,_bZ],_c1=[1,_b0,_c0],_c2=[1,_aV,_c1],_c3=[1,_aQ,_c2],_c4=[1,_aL,_c3],_c5=[1,_aG,_c4],_c6=[1,_aB,_c5],_c7=[1,_aw,_c6],_c8=[1,_ar,_c7],_c9=[1,_am,_c8],_ca=[1,_ah,_c9],_cb=[1,_ac,_ca],_cc=[1,_a7,_cb],_cd=[1,_a2,_cc],_ce=[1,_9X,_cd],_cf=[1,_9S,_ce],_cg=[1,_9N,_cf],_ch=[1,_9I,_cg],_ci=[1,_9D,_ch],_cj=[1,_9y,_ci],_ck=[1,_9t,_cj],_cl=[1,_9o,_ck],_cm=[1,_9j,_cl],_cn=unCStr("SOH"),_co=[0,1],_cp=function(_cq){return _96(_cn,function(_cr){return A(_cq,[_co]);});},_cs=unCStr("SO"),_ct=[0,14],_cu=function(_cv){return _96(_cs,function(_cw){return A(_cv,[_ct]);});},_cx=function(_cy){return _6A(_cp,_cu,_cy);},_cz=[1,_cx,_cm],_cA=new T(function(){return _8V(_cz);}),_cB=[0,1114111],_cC=[0,34],_cD=[0,_cC,_8p],_cE=[0,39],_cF=[0,_cE,_8p],_cG=[0,92],_cH=[0,_cG,_8p],_cI=[0,_9M,_8p],_cJ=[0,_9R,_8p],_cK=[0,_ab,_8p],_cL=[0,_a1,_8p],_cM=[0,_ag,_8p],_cN=[0,_9W,_8p],_cO=[0,_a6,_8p],_cP=[0,_9i,_8p],_cQ=[0,_co,_8p],_cR=[0,_9n,_8p],_cS=[0,_9s,_8p],_cT=[0,_9x,_8p],_cU=[0,_9C,_8p],_cV=[0,_9H,_8p],_cW=[0,_9M,_8p],_cX=[0,_9R,_8p],_cY=[0,_9W,_8p],_cZ=[0,_a1,_8p],_d0=[0,_a6,_8p],_d1=[0,_ab,_8p],_d2=[0,_ag,_8p],_d3=[0,_ct,_8p],_d4=[0,_al,_8p],_d5=[0,_aq,_8p],_d6=[0,_av,_8p],_d7=[0,_aA,_8p],_d8=[0,_aF,_8p],_d9=[0,_aK,_8p],_da=[0,_aP,_8p],_db=[0,_aU,_8p],_dc=[0,_aZ,_8p],_dd=[0,_b4,_8p],_de=[0,_b9,_8p],_df=[0,_be,_8p],_dg=[0,_bj,_8p],_dh=[0,_bo,_8p],_di=[0,_bt,_8p],_dj=[0,_by,_8p],_dk=[0,_bD,_8p],_dl=function(_dm){return [0,_dm];},_dn=function(_do){return _5C([0,function(_dp){switch(E(E(_dp)[1])){case 34:return A(_do,[_cD]);case 39:return A(_do,[_cF]);case 92:return A(_do,[_cH]);case 97:return A(_do,[_cI]);case 98:return A(_do,[_cJ]);case 102:return A(_do,[_cK]);case 110:return A(_do,[_cL]);case 114:return A(_do,[_cM]);case 116:return A(_do,[_cN]);case 118:return A(_do,[_cO]);default:return [2];}}],new T(function(){return _5C(_6A(_8q,_8t,function(_dq){return _6Z(_dq,function(_dr){var _ds=_7H(new T(function(){return _dl(E(_dq)[1]);}),_7y,_dr);return !_8L(_ds,_cB)?[2]:A(_do,[[0,new T(function(){var _dt=_8I(_ds);return _dt>>>0>1114111?_8G(_dt):[0,_dt];}),_8p]]);});}),new T(function(){return _5C([0,function(_du){return E(E(_du)[1])==94?E([0,function(_dv){switch(E(E(_dv)[1])){case 64:return A(_do,[_cP]);case 65:return A(_do,[_cQ]);case 66:return A(_do,[_cR]);case 67:return A(_do,[_cS]);case 68:return A(_do,[_cT]);case 69:return A(_do,[_cU]);case 70:return A(_do,[_cV]);case 71:return A(_do,[_cW]);case 72:return A(_do,[_cX]);case 73:return A(_do,[_cY]);case 74:return A(_do,[_cZ]);case 75:return A(_do,[_d0]);case 76:return A(_do,[_d1]);case 77:return A(_do,[_d2]);case 78:return A(_do,[_d3]);case 79:return A(_do,[_d4]);case 80:return A(_do,[_d5]);case 81:return A(_do,[_d6]);case 82:return A(_do,[_d7]);case 83:return A(_do,[_d8]);case 84:return A(_do,[_d9]);case 85:return A(_do,[_da]);case 86:return A(_do,[_db]);case 87:return A(_do,[_dc]);case 88:return A(_do,[_dd]);case 89:return A(_do,[_de]);case 90:return A(_do,[_df]);case 91:return A(_do,[_dg]);case 92:return A(_do,[_dh]);case 93:return A(_do,[_di]);case 94:return A(_do,[_dj]);case 95:return A(_do,[_dk]);default:return [2];}}]):[2];}],new T(function(){return A(_cA,[function(_dw){return A(_do,[[0,_dw,_8p]]);}]);}));}));}));},_dx=function(_dy){return A(_dy,[_0]);},_dz=function(_dA){var _dB=E(_dA);if(!_dB[0]){return E(_dx);}else{var _dC=_dB[2],_dD=E(E(_dB[1])[1]);switch(_dD){case 9:return function(_dE){return [0,function(_dF){return A(_dz(_dC),[_dE]);}];};case 10:return function(_dG){return [0,function(_dH){return A(_dz(_dC),[_dG]);}];};case 11:return function(_dI){return [0,function(_dJ){return A(_dz(_dC),[_dI]);}];};case 12:return function(_dK){return [0,function(_dL){return A(_dz(_dC),[_dK]);}];};case 13:return function(_dM){return [0,function(_dN){return A(_dz(_dC),[_dM]);}];};case 32:return function(_dO){return [0,function(_dP){return A(_dz(_dC),[_dO]);}];};case 160:return function(_dQ){return [0,function(_dR){return A(_dz(_dC),[_dQ]);}];};default:var _dS=u_iswspace(_dD);return E(_dS)==0?E(_dx):function(_dT){return [0,function(_dU){return A(_dz(_dC),[_dT]);}];};}}},_dV=function(_dW){var _dX=new T(function(){return _dV(_dW);}),_dY=[1,function(_dZ){return A(_dz,[_dZ,function(_e0){return E([0,function(_e1){return E(E(_e1)[1])==92?E(_dX):[2];}]);}]);}];return _5C([0,function(_e2){return E(E(_e2)[1])==92?E([0,function(_e3){var _e4=E(E(_e3)[1]);switch(_e4){case 9:return E(_dY);case 10:return E(_dY);case 11:return E(_dY);case 12:return E(_dY);case 13:return E(_dY);case 32:return E(_dY);case 38:return E(_dX);case 160:return E(_dY);default:var _e5=u_iswspace(_e4);return E(_e5)==0?[2]:E(_dY);}}]):[2];}],[0,function(_e6){var _e7=E(_e6);return E(_e7[1])==92?_dn(_dW):A(_dW,[[0,_e7,_8o]]);}]);},_e8=function(_e9,_ea){return _dV(function(_eb){var _ec=E(_eb),_ed=E(_ec[1]);return E(_ed[1])==34?!E(_ec[2])?A(_ea,[[1,new T(function(){return A(_e9,[_k]);})]]):_e8(function(_ee){return A(_e9,[[1,_ed,_ee]]);},_ea):_e8(function(_ef){return A(_e9,[[1,_ed,_ef]]);},_ea);});},_eg=unCStr("_\'"),_eh=function(_ei){var _ej=u_iswalnum(_ei);return E(_ej)==0?_1U(_69,[0,_ei],_eg):true;},_ek=function(_el){return _eh(E(_el)[1]);},_em=unCStr(",;()[]{}`"),_en=function(_eo){return A(_eo,[_k]);},_ep=function(_eq,_er){var _es=function(_et){var _eu=E(_et);if(!_eu[0]){return E(_en);}else{var _ev=_eu[1];return !A(_eq,[_ev])?E(_en):function(_ew){return [0,function(_ex){return A(_es(_eu[2]),[function(_ey){return A(_ew,[[1,_ev,_ey]]);}]);}];};}};return [1,function(_ez){return A(_es,[_ez,_er]);}];},_eA=unCStr(".."),_eB=unCStr("::"),_eC=unCStr("->"),_eD=[0,64],_eE=[1,_eD,_k],_eF=[0,126],_eG=[1,_eF,_k],_eH=unCStr("=>"),_eI=[1,_eH,_k],_eJ=[1,_eG,_eI],_eK=[1,_eE,_eJ],_eL=[1,_eC,_eK],_eM=unCStr("<-"),_eN=[1,_eM,_eL],_eO=[0,124],_eP=[1,_eO,_k],_eQ=[1,_eP,_eN],_eR=[1,_cG,_k],_eS=[1,_eR,_eQ],_eT=[0,61],_eU=[1,_eT,_k],_eV=[1,_eU,_eS],_eW=[1,_eB,_eV],_eX=[1,_eA,_eW],_eY=function(_eZ){return _5C([1,function(_f0){return E(_f0)[0]==0?A(_eZ,[_6U]):[2];}],new T(function(){return _5C([0,function(_f1){return E(E(_f1)[1])==39?E([0,function(_f2){var _f3=E(_f2);switch(E(_f3[1])){case 39:return [2];case 92:return _dn(function(_f4){var _f5=E(_f4);return (function(_f6,_f7){var _f8=new T(function(){return A(_eZ,[[0,_f6]]);});return !E(_f7)?E(E(_f6)[1])==39?[2]:[0,function(_f9){return E(E(_f9)[1])==39?E(_f8):[2];}]:[0,function(_fa){return E(E(_fa)[1])==39?E(_f8):[2];}];})(_f5[1],_f5[2]);});default:return [0,function(_fb){return E(E(_fb)[1])==39?A(_eZ,[[0,_f3]]):[2];}];}}]):[2];}],new T(function(){return _5C([0,function(_fc){return E(E(_fc)[1])==34?_e8(_6V,_eZ):[2];}],new T(function(){return _5C([0,function(_fd){return !_1U(_69,_fd,_em)?[2]:A(_eZ,[[2,[1,_fd,_k]]]);}],new T(function(){return _5C([0,function(_fe){return !_1U(_69,_fe,_8b)?[2]:_ep(_8c,function(_ff){var _fg=[1,_fe,_ff];return !_1U(_6g,_fg,_eX)?A(_eZ,[[4,_fg]]):A(_eZ,[[2,_fg]]);});}],new T(function(){return _5C([0,function(_fh){var _fi=E(_fh),_fj=_fi[1],_fk=u_iswalpha(_fj);return E(_fk)==0?E(_fj)==95?_ep(_ek,function(_fl){return A(_eZ,[[3,[1,_fi,_fl]]]);}):[2]:_ep(_ek,function(_fm){return A(_eZ,[[3,[1,_fi,_fm]]]);});}],new T(function(){return _6A(_8g,_86,_eZ);}));}));}));}));}));}));},_fn=function(_fo){return [1,function(_fp){return A(_dz,[_fp,function(_fq){return _eY(_fo);}]);}];},_fr=[0,0],_fs=function(_ft,_fu){return _fn(function(_fv){var _fw=E(_fv);if(_fw[0]==2){var _fx=E(_fw[1]);return _fx[0]==0?[2]:E(E(_fx[1])[1])==40?E(_fx[2])[0]==0?A(_ft,[_fr,function(_fy){return _fn(function(_fz){var _fA=E(_fz);if(_fA[0]==2){var _fB=E(_fA[1]);return _fB[0]==0?[2]:E(E(_fB[1])[1])==41?E(_fB[2])[0]==0?A(_fu,[_fy]):[2]:[2];}else{return [2];}});}]):[2]:[2];}else{return [2];}});},_fC=function(_fD,_fE,_fF){var _fG=function(_fH,_fI){return _5C(_fn(function(_fJ){var _fK=E(_fJ);if(_fK[0]==4){var _fL=E(_fK[1]);return _fL[0]==0?A(_fD,[_fK,_fH,_fI]):E(E(_fL[1])[1])==45?E(_fL[2])[0]==0?E([1,function(_fM){return A(_dz,[_fM,function(_fN){return _eY(function(_fO){return A(_fD,[_fO,_fH,function(_fP){return A(_fI,[new T(function(){return [0, -E(_fP)[1]];})]);}]);});}]);}]):A(_fD,[_fK,_fH,_fI]):A(_fD,[_fK,_fH,_fI]);}else{return A(_fD,[_fK,_fH,_fI]);}}),new T(function(){return _fs(_fG,_fI);}));};return _fG(_fE,_fF);},_fQ=function(_fR,_fS){return [2];},_fT=function(_fU,_fV){return _fQ(_fU,_fV);},_fW=function(_fX){var _fY=E(_fX);return _fY[0]==0?[1,new T(function(){return _7H(new T(function(){return _dl(E(_fY[1])[1]);}),_7y,_fY[2]);})]:E(_fY[2])[0]==0?E(_fY[3])[0]==0?[1,new T(function(){return _7H(_7x,_7y,_fY[1]);})]:[0]:[0];},_fZ=function(_g0){var _g1=E(_g0);if(_g1[0]==5){var _g2=_fW(_g1[1]);return _g2[0]==0?E(_fQ):function(_g3,_g4){return A(_g4,[new T(function(){return [0,_8I(_g2[1])];})]);};}else{return E(_fT);}},_g5=function(_g6){return [1,function(_g7){return A(_dz,[_g7,function(_g8){return E([3,_g6,_6s]);}]);}];},_g9=new T(function(){return _fC(_fZ,_fr,_g5);}),_ga=function(_gb){while(1){var _gc=(function(_gd){var _ge=E(_gd);if(!_ge[0]){return [0];}else{var _gf=_ge[2],_gg=E(_ge[1]);if(!E(_gg[2])[0]){return [1,_gg[1],new T(function(){return _ga(_gf);})];}else{_gb=_gf;return null;}}})(_gb);if(_gc!=null){return _gc;}}},_gh=function(_gi){var _gj=new T(function(){return _57(_gi,45);});return [0,new T(function(){var _gk=_ga(_5s(_g9,new T(function(){return _4S(_gj,0);})));return _gk[0]==0?E(_5q):E(_gk[2])[0]==0?E(_gk[1]):E(_5o);}),new T(function(){var _gl=_ga(_5s(_g9,new T(function(){return _4S(_gj,1);})));return _gl[0]==0?E(_5q):E(_gl[2])[0]==0?E(_gl[1]):E(_5o);})];},_gm=new T(function(){return _4S(_5f,1);}),_gn=new T(function(){var _go=_gh(_gm);return [0,_go[1],_go[2]];}),_gp=new T(function(){return _4S(_5f,2);}),_gq=new T(function(){return _3p("UI.hs:(49,1)-(53,42)|function modelByState");}),_gr=unCStr("submarine"),_gs=unCStr("patrolboat"),_gt=unCStr("destroyer"),_gu=unCStr("battleship"),_gv=unCStr("aircraftcarrier"),_gw=function(_gx){return !_5h(_gx,_gv)?!_5h(_gx,_gu)?!_5h(_gx,_gt)?!_5h(_gx,_gs)?!_5h(_gx,_gr)?E(_gq):2:4:3:1:0;},_gy=new T(function(){return _gw(_gp);}),_gz=[0,_gy,_gn,_5m],_gA=new T(function(){return _4o(_k,_gz);}),_gB=function(_gC,_){if(!E(_gA)){jsAlert(toJSStr(E(_1)));return _0;}else{selectBoat(E(_gC)[1]);return _0;}},_gD=[0,_gB],_gE=unCStr("tbody td:not(.shead)"),_gF=function(_gG,_){flipBoat(E(_gG)[1]);return _0;},_gH=function(_gI,_){return _gF(_gI,_);},_gJ=[0,_gH],_gK=unCStr("button#flip"),_gL=function(_gM,_){selectBoat(E(_gM)[1]);return _0;},_gN=function(_gI,_){return _gL(_gI,_);},_gO=[0,_gN],_gP=unCStr(".boat"),_gQ=function(_gR,_){resetGame(E(_gR)[1]);return _0;},_gS=function(_gI,_){return _gQ(_gI,_);},_gT=[0,_gS],_gU=unCStr("button#reset"),_gV=function(_gW,_){startGame(E(_gW)[1]);return _0;},_gX=function(_gI,_){return _gV(_gI,_);},_gY=[0,_gX],_gZ=unCStr("button#start"),_h0=function(_h1,_){lockBoat(E(_h1)[1]);return _0;},_h2=function(_gI,_){return _h0(_gI,_);},_h3=[0,_h2],_h4=unCStr("button#lock"),_h5=function(_){var _h6=E(_gP),_h7=js_jquery(toJSStr(_h6)),_h8=E(_gO)[1];js_click(_h7,_h8);var _h9=js_jquery(toJSStr(E(_gK)));js_click(_h9,E(_gJ)[1]);var _ha=js_jquery(toJSStr(E(_h4)));js_click(_ha,E(_h3)[1]);var _hb=js_jquery(toJSStr(E(_gZ)));js_click(_hb,E(_gY)[1]);var _hc=js_jquery(toJSStr(E(_gU)));js_click(_hc,E(_gT)[1]);var _hd=js_jquery(toJSStr(_h6));js_click(_hd,_h8);var _he=js_jquery(toJSStr(E(_gE)));js_click(_he,E(_gD)[1]);return _0;},_hf=function(_){return _h5(_);};
var hasteMain = function() {A(_hf, [0]);};window.onload = hasteMain;