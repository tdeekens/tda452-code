// jQuery can't be dollar bills in Haskell, this is its export
var js_jquery = $;

// Imported within Haskell's UI.hs to bind a click handler with callback
function js_click(obj, callback) {
   obj.click(function(evt) {
      A(
         callback,
         [
            // ... callbacks get the DOM Node's id to identify them
            [0, "#" + $(this).attr('id')],
            [0, getState()]
         , 0]
      );
   });
}

function js_unbind(obj) {
   obj.off();
}

// Config values used as defaults
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

// State is maintained within the object, always contains
// last selected boat also fetched by Haskell as String
state = {
   boatmodel: undefined,
   start: undefined,
   alignment: undefined
};

// Selects a boat from the legend (without coloring anything)
// just changes state within JS
selectBoat = function(idx) {
   var $this     = $(idx);
   var boatmodel = $this.attr('id');

   state.boatmodel = boatmodel;
   state.start     = config.start;
   state.alignment = config.alignment;

   console.log('State changed:', state);
};

// Bound by Haskell to get the current UI's state as String
getState = function() {
   var start = "";

   if (state.start !== undefined) {
      start = state.start.join("-");
   }

   return state.boatmodel + "|" + start + "|" + state.alignment;
}

// Adds a boat to the field by coloring cells, also sets state's start
addBoat = function(idx) {
   if (state.boatmodel === undefined) {
      alert("Please select a boat to position!");
      return false;
   }

   markHorizontally(false);

   var $this      = $(idx)
       , id       = $this.attr('id')
       , position = id.split(config.delimiter);

   state.start = [position[0]--, position[1]--];

   markHorizontally(true);

   console.log('State changed:', state);
}

// Colors/uncolors cells horitzonally based on the book flag
markHorizontally = function(book) {
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

// Colors/uncolors cells vertically based on the book flag
markVertically = function(book) {
   var boatmodel = state.boatmodel;

   var $rows     = $('tr')
       , column = state.start[1] + 1
       , row  = state.start[0] + 1

   var length = row + config.sizes[boatmodel];

   $rows = $rows.slice(row, length);

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

// Flips a boat's alignment and colors cells accordingly
flipBoat = function() {
   if(state.alignment === 'horizontal') {
      state.alignment = 'vertical';
      markHorizontally(false);
      markVertically(true);
   } else {
      state.alignment = 'horizontal';
      markVertically(false);
      markHorizontally(true);
   }

   console.log('State changed:', state);
};

// Locks a boat by resetting the state to initial representation
lockBoat = function() {
   state.boatmodel = undefined;
   state.start     = undefined;
   state.alignment = undefined;
};

// Clears the field to start the game
startGame = function(idx) {
   var $tds = $('tbody td:not(.shead)');

   $tds.removeClass('boat');

   $('#legend').css('visibility', 'hidden');
   $('table').addClass('game-mode');
};

// Marks a hit on the field
markHit = function(idx) {
   var $this = $("#" + idx);

   $this.text('☠');
};

// Marks a hit on the field
markMiss = function(idx) {
   var $this = $("#" + idx);

   $this.text('☹');
};

// Though one: refreshes the browser window to restart game
resetGame = function(idx) {
   window.location.reload()
};

// Outputs a string within the game's message box
message = function(msg) {
   $msgBox = $("#msg-box");

   $msgBox.html(msg);
}

// Outputs a string within the game's debug box
debug = function(msg) {
   $debugBox = $("#debug-box");

   $debugBox.prepend("<br />" + msg);
}
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

var _0=0,_1=[0,44],_2=function(_3,_4,_5){return A(_3,[[1,_1,new T(function(){return A(_4,[_5]);})]]);},_6=unCStr("Horizontal"),_7=unCStr("Vertical"),_8=[0,125],_9=unCStr("AircraftCarrier"),_a=unCStr("PatrolBoat"),_b=unCStr("Destroyer"),_c=unCStr("Submarine"),_d=unCStr("Battleship"),_e=function(_f,_g){var _h=E(_f);return _h[0]==0?E(_g):[1,_h[1],new T(function(){return _e(_h[2],_g);})];},_i=function(_j,_k){var _l=jsShowI(_j);return _e(fromJSStr(_l),_k);},_m=[0,41],_n=[0,40],_o=function(_p,_q,_r){return _q>=0?_i(_q,_r):_p<=6?_i(_q,_r):[1,_n,new T(function(){var _s=jsShowI(_q);return _e(fromJSStr(_s),[1,_m,_r]);})];},_t=[0],_u=unCStr(": empty list"),_v=unCStr("Prelude."),_w=function(_x){return err(_e(_v,new T(function(){return _e(_x,_u);})));},_y=unCStr("foldr1"),_z=new T(function(){return _w(_y);}),_A=function(_B,_C){var _D=E(_C);if(!_D[0]){return E(_z);}else{var _E=_D[1],_F=E(_D[2]);return _F[0]==0?E(_E):A(_B,[_E,new T(function(){return _A(_B,_F);})]);}},_G=unCStr("Boat {"),_H=unCStr("model = "),_I=unCStr(", "),_J=unCStr("start = "),_K=unCStr("alignment = "),_L=function(_M,_N,_O,_P,_Q){var _R=function(_S){return _e(_G,new T(function(){return _e(_H,new T(function(){var _T=new T(function(){return _e(_I,new T(function(){return _e(_J,new T(function(){var _U=E(_O);return [1,_n,new T(function(){return A(_A,[_2,[1,function(_V){return _o(0,E(_U[1])[1],_V);},[1,function(_W){return _o(0,E(_U[2])[1],_W);},_t]],[1,_m,new T(function(){return _e(_I,new T(function(){return _e(_K,new T(function(){return E(_P)==0?_e(_7,[1,_8,_S]):_e(_6,[1,_8,_S]);}));}));})]]);})];}));}));});switch(E(_N)){case 0:return _e(_9,_T);case 1:return _e(_d,_T);case 2:return _e(_c,_T);case 3:return _e(_b,_T);default:return _e(_a,_T);}}));}));};return _M<11?_R(_Q):[1,_n,new T(function(){return _R([1,_m,_Q]);})];},_X=unCStr("Prelude.(!!): index too large\n"),_Y=new T(function(){return err(_X);}),_Z=function(_10,_11){while(1){var _12=E(_10);if(!_12[0]){return E(_Y);}else{var _13=E(_11);if(!_13){return E(_12[1]);}else{_10=_12[2];_11=_13-1|0;continue;}}}},_14=unCStr("Prelude.read: ambiguous parse"),_15=new T(function(){return err(_14);}),_16=unCStr("Prelude.read: no parse"),_17=new T(function(){return err(_16);}),_18=unCStr("base"),_19=unCStr("Control.Exception.Base"),_1a=unCStr("PatternMatchFail"),_1b=[0,18445595,52003073,_18,_19,_1a],_1c=[0,18445595,52003073,_1b,_t],_1d=function(_1e){return E(_1c);},_1f=function(_1g){return E(E(_1g)[1]);},_1h=unCStr("Maybe.fromJust: Nothing"),_1i=new T(function(){return err(_1h);}),_1j=function(_1k,_1l,_1m){var _1n=new T(function(){var _1o=A(_1k,[_1m]),_1p=A(_1l,[new T(function(){var _1q=E(_1n);return _1q[0]==0?E(_1i):E(_1q[1]);})]),_1r=hs_eqWord64(_1o[1],_1p[1]);if(!E(_1r)){return [0];}else{var _1s=hs_eqWord64(_1o[2],_1p[2]);return E(_1s)==0?[0]:[1,_1m];}});return E(_1n);},_1t=function(_1u){var _1v=E(_1u);return _1j(_1f(_1v[1]),_1d,_1v[2]);},_1w=function(_1x){return E(E(_1x)[1]);},_1y=function(_1z,_1A){return _e(E(_1z)[1],_1A);},_1B=[0,93],_1C=[0,91],_1D=function(_1E,_1F,_1G){var _1H=E(_1F);return _1H[0]==0?unAppCStr("[]",_1G):[1,_1C,new T(function(){return A(_1E,[_1H[1],new T(function(){var _1I=function(_1J){var _1K=E(_1J);return _1K[0]==0?E([1,_1B,_1G]):[1,_1,new T(function(){return A(_1E,[_1K[1],new T(function(){return _1I(_1K[2]);})]);})];};return _1I(_1H[2]);})]);})];},_1L=function(_1M,_1N){return _1D(_1y,_1M,_1N);},_1O=function(_1P,_1Q,_1R){return _e(E(_1Q)[1],_1R);},_1S=[0,_1O,_1w,_1L],_1T=[0,_1d,_1S,_1U,_1t],_1U=function(_1V){return [0,_1T,_1V];},_1W=unCStr("Non-exhaustive patterns in"),_1X=function(_1Y,_1Z){return die(new T(function(){return A(_1Z,[_1Y]);}));},_20=function(_21,_22){var _23=E(_22);if(!_23[0]){return [0,_t,_t];}else{var _24=_23[1];if(!A(_21,[_24])){return [0,_t,_23];}else{var _25=new T(function(){var _26=_20(_21,_23[2]);return [0,_26[1],_26[2]];});return [0,[1,_24,new T(function(){return E(E(_25)[1]);})],new T(function(){return E(E(_25)[2]);})];}}},_27=[0,32],_28=[0,10],_29=[1,_28,_t],_2a=function(_2b){return E(E(_2b)[1])==124?false:true;},_2c=function(_2d,_2e){var _2f=_20(_2a,unCStr(_2d)),_2g=_2f[1],_2h=function(_2i,_2j){return _e(_2i,new T(function(){return unAppCStr(": ",new T(function(){return _e(_2e,new T(function(){return _e(_2j,_29);}));}));}));},_2k=E(_2f[2]);return _2k[0]==0?_2h(_2g,_t):E(E(_2k[1])[1])==124?_2h(_2g,[1,_27,_2k[2]]):_2h(_2g,_t);},_2l=function(_2m){return _1X([0,new T(function(){return _2c(_2m,_1W);})],_1U);},_2n=new T(function(){return _2l("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus");}),_2o=function(_2p,_2q){while(1){var _2r=(function(_2s,_2t){var _2u=E(_2s);switch(_2u[0]){case 0:var _2v=E(_2t);if(!_2v[0]){return [0];}else{_2p=A(_2u[1],[_2v[1]]);_2q=_2v[2];return null;}break;case 1:var _2w=A(_2u[1],[_2t]),_2x=_2t;_2p=_2w;_2q=_2x;return null;case 2:return [0];case 3:return [1,[0,_2u[1],_2t],new T(function(){return _2o(_2u[2],_2t);})];default:return E(_2u[1]);}})(_2p,_2q);if(_2r!=null){return _2r;}}},_2y=function(_2z,_2A){var _2B=new T(function(){var _2C=E(_2A);if(_2C[0]==3){return [3,_2C[1],new T(function(){return _2y(_2z,_2C[2]);})];}else{var _2D=E(_2z);if(_2D[0]==2){return E(_2C);}else{var _2E=E(_2C);if(_2E[0]==2){return E(_2D);}else{var _2F=new T(function(){var _2G=E(_2E);if(_2G[0]==4){return [1,function(_2H){return [4,new T(function(){return _e(_2o(_2D,_2H),_2G[1]);})];}];}else{var _2I=E(_2D);if(_2I[0]==1){var _2J=_2I[1],_2K=E(_2G);return _2K[0]==0?[1,function(_2L){return _2y(A(_2J,[_2L]),_2K);}]:[1,function(_2M){return _2y(A(_2J,[_2M]),new T(function(){return A(_2K[1],[_2M]);}));}];}else{var _2N=E(_2G);return _2N[0]==0?E(_2n):[1,function(_2O){return _2y(_2I,new T(function(){return A(_2N[1],[_2O]);}));}];}}}),_2P=E(_2D);switch(_2P[0]){case 1:var _2Q=E(_2E);return _2Q[0]==4?[1,function(_2R){return [4,new T(function(){return _e(_2o(A(_2P[1],[_2R]),_2R),_2Q[1]);})];}]:E(_2F);case 4:var _2S=_2P[1],_2T=E(_2E);switch(_2T[0]){case 0:return [1,function(_2U){return [4,new T(function(){return _e(_2S,new T(function(){return _2o(_2T,_2U);}));})];}];case 1:return [1,function(_2V){return [4,new T(function(){return _e(_2S,new T(function(){return _2o(A(_2T[1],[_2V]),_2V);}));})];}];default:return [4,new T(function(){return _e(_2S,_2T[1]);})];}break;default:return E(_2F);}}}}}),_2W=E(_2z);switch(_2W[0]){case 0:var _2X=E(_2A);return _2X[0]==0?[0,function(_2Y){return _2y(A(_2W[1],[_2Y]),new T(function(){return A(_2X[1],[_2Y]);}));}]:E(_2B);case 3:return [3,_2W[1],new T(function(){return _2y(_2W[2],_2A);})];default:return E(_2B);}},_2Z=function(_30,_31){return E(_30)[1]!=E(_31)[1];},_32=function(_33,_34){return E(_33)[1]==E(_34)[1];},_35=[0,_32,_2Z],_36=function(_37){return E(E(_37)[1]);},_38=function(_39,_3a,_3b){while(1){var _3c=E(_3a);if(!_3c[0]){return E(_3b)[0]==0?true:false;}else{var _3d=E(_3b);if(!_3d[0]){return false;}else{if(!A(_36,[_39,_3c[1],_3d[1]])){return false;}else{_3a=_3c[2];_3b=_3d[2];continue;}}}}},_3e=function(_3f,_3g,_3h){return !_38(_3f,_3g,_3h)?true:false;},_3i=function(_3j){return [0,function(_3k,_3l){return _38(_3j,_3k,_3l);},function(_3k,_3l){return _3e(_3j,_3k,_3l);}];},_3m=new T(function(){return _3i(_35);}),_3n=function(_3o,_3p){var _3q=E(_3o);switch(_3q[0]){case 0:return [0,function(_3r){return _3n(A(_3q[1],[_3r]),_3p);}];case 1:return [1,function(_3s){return _3n(A(_3q[1],[_3s]),_3p);}];case 2:return [2];case 3:return _2y(A(_3p,[_3q[1]]),new T(function(){return _3n(_3q[2],_3p);}));default:var _3t=function(_3u){var _3v=E(_3u);if(!_3v[0]){return [0];}else{var _3w=E(_3v[1]);return _e(_2o(A(_3p,[_3w[1]]),_3w[2]),new T(function(){return _3t(_3v[2]);}));}},_3x=_3t(_3q[1]);return _3x[0]==0?[2]:[4,_3x];}},_3y=[2],_3z=function(_3A){return [3,_3A,_3y];},_3B=function(_3C,_3D){var _3E=E(_3C);return _3E==0?A(_3D,[_0]):[0,function(_3F){return _3B(_3E-1|0,_3D);}];},_3G=function(_3H,_3I,_3J){return [1,function(_3K){return A(function(_3L,_3M,_3N){while(1){var _3O=(function(_3P,_3Q,_3R){var _3S=E(_3P);switch(_3S[0]){case 0:var _3T=E(_3Q);if(!_3T[0]){return E(_3I);}else{_3L=A(_3S[1],[_3T[1]]);_3M=_3T[2];var _3U=_3R+1|0;_3N=_3U;return null;}break;case 1:var _3V=A(_3S[1],[_3Q]),_3W=_3Q,_3U=_3R;_3L=_3V;_3M=_3W;_3N=_3U;return null;case 2:return E(_3I);case 3:return function(_3X){return _3B(_3R,function(_3Y){return _3n(_3S,_3X);});};default:return function(_3Z){return _3n(_3S,_3Z);};}})(_3L,_3M,_3N);if(_3O!=null){return _3O;}}},[new T(function(){return A(_3H,[_3z]);}),_3K,0,_3J]);}];},_40=[6],_41=function(_42){return E(_42);},_43=unCStr("valDig: Bad base"),_44=new T(function(){return err(_43);}),_45=function(_46,_47){var _48=function(_49,_4a){var _4b=E(_49);if(!_4b[0]){return function(_4c){return A(_4c,[new T(function(){return A(_4a,[_t]);})]);};}else{var _4d=E(_4b[1])[1],_4e=function(_4f){return function(_4g){return [0,function(_4h){return A(_48(_4b[2],function(_4i){return A(_4a,[[1,_4f,_4i]]);}),[_4g]);}];};};switch(E(E(_46)[1])){case 8:return 48>_4d?function(_4j){return A(_4j,[new T(function(){return A(_4a,[_t]);})]);}:_4d>55?function(_4k){return A(_4k,[new T(function(){return A(_4a,[_t]);})]);}:_4e([0,_4d-48|0]);case 10:return 48>_4d?function(_4l){return A(_4l,[new T(function(){return A(_4a,[_t]);})]);}:_4d>57?function(_4m){return A(_4m,[new T(function(){return A(_4a,[_t]);})]);}:_4e([0,_4d-48|0]);case 16:var _4n=new T(function(){return 97>_4d?65>_4d?[0]:_4d>70?[0]:[1,[0,(_4d-65|0)+10|0]]:_4d>102?65>_4d?[0]:_4d>70?[0]:[1,[0,(_4d-65|0)+10|0]]:[1,[0,(_4d-97|0)+10|0]];});if(48>_4d){var _4o=E(_4n);return _4o[0]==0?function(_4p){return A(_4p,[new T(function(){return A(_4a,[_t]);})]);}:_4e(_4o[1]);}else{if(_4d>57){var _4q=E(_4n);return _4q[0]==0?function(_4r){return A(_4r,[new T(function(){return A(_4a,[_t]);})]);}:_4e(_4q[1]);}else{return _4e([0,_4d-48|0]);}}break;default:return E(_44);}}};return [1,function(_4s){return A(_48,[_4s,_41,function(_4t){var _4u=E(_4t);return _4u[0]==0?[2]:A(_47,[_4u]);}]);}];},_4v=[0,10],_4w=[0,1],_4x=[0,2147483647],_4y=function(_4z,_4A){while(1){var _4B=E(_4z);if(!_4B[0]){var _4C=_4B[1],_4D=E(_4A);if(!_4D[0]){var _4E=_4D[1],_4F=addC(_4C,_4E);if(!E(_4F[2])){return [0,_4F[1]];}else{_4z=[1,I_fromInt(_4C)];_4A=[1,I_fromInt(_4E)];continue;}}else{_4z=[1,I_fromInt(_4C)];_4A=_4D;continue;}}else{var _4G=E(_4A);if(!_4G[0]){_4z=_4B;_4A=[1,I_fromInt(_4G[1])];continue;}else{return [1,I_add(_4B[1],_4G[1])];}}}},_4H=new T(function(){return _4y(_4x,_4w);}),_4I=function(_4J){var _4K=E(_4J);if(!_4K[0]){var _4L=E(_4K[1]);return _4L==(-2147483648)?E(_4H):[0, -_4L];}else{return [1,I_negate(_4K[1])];}},_4M=[0,10],_4N=[0,0],_4O=function(_4P,_4Q){while(1){var _4R=E(_4P);if(!_4R[0]){var _4S=_4R[1],_4T=E(_4Q);if(!_4T[0]){var _4U=_4T[1];if(!(imul(_4S,_4U)|0)){return [0,imul(_4S,_4U)|0];}else{_4P=[1,I_fromInt(_4S)];_4Q=[1,I_fromInt(_4U)];continue;}}else{_4P=[1,I_fromInt(_4S)];_4Q=_4T;continue;}}else{var _4V=E(_4Q);if(!_4V[0]){_4P=_4R;_4Q=[1,I_fromInt(_4V[1])];continue;}else{return [1,I_mul(_4R[1],_4V[1])];}}}},_4W=function(_4X,_4Y,_4Z){while(1){var _50=E(_4Z);if(!_50[0]){return E(_4Y);}else{var _51=_4y(_4O(_4Y,_4X),_50[1]);_4Z=_50[2];_4Y=_51;continue;}}},_52=function(_53){var _54=new T(function(){return _2y(_2y([0,function(_55){return E(E(_55)[1])==45?_45(_4v,function(_56){return A(_53,[[1,new T(function(){return _4I(_4W(_4M,_4N,_56));})]]);}):[2];}],[0,function(_57){return E(E(_57)[1])==43?_45(_4v,function(_58){return A(_53,[[1,new T(function(){return _4W(_4M,_4N,_58);})]]);}):[2];}]),new T(function(){return _45(_4v,function(_59){return A(_53,[[1,new T(function(){return _4W(_4M,_4N,_59);})]]);});}));});return _2y([0,function(_5a){return E(E(_5a)[1])==101?E(_54):[2];}],[0,function(_5b){return E(E(_5b)[1])==69?E(_54):[2];}]);},_5c=[0],_5d=function(_5e){return A(_5e,[_5c]);},_5f=function(_5g){return A(_5g,[_5c]);},_5h=function(_5i){return [0,function(_5j){return E(E(_5j)[1])==46?_45(_4v,function(_5k){return A(_5i,[[1,_5k]]);}):[2];}];},_5l=function(_5m){return _45(_4v,function(_5n){return _3G(_5h,_5d,function(_5o){return _3G(_52,_5f,function(_5p){return A(_5m,[[5,[1,_5n,_5o,_5p]]]);});});});},_5q=function(_5r,_5s,_5t){while(1){var _5u=E(_5t);if(!_5u[0]){return false;}else{if(!A(_36,[_5r,_5s,_5u[1]])){_5t=_5u[2];continue;}else{return true;}}}},_5v=unCStr("!@#$%&*+./<=>?\\^|:-~"),_5w=function(_5x){return _5q(_35,_5x,_5v);},_5y=[0,8],_5z=[0,16],_5A=function(_5B){return [0,function(_5C){return E(E(_5C)[1])==48?E([0,function(_5D){switch(E(E(_5D)[1])){case 79:return _45(_5y,function(_5E){return A(_5B,[[5,[0,_5y,_5E]]]);});case 88:return _45(_5z,function(_5F){return A(_5B,[[5,[0,_5z,_5F]]]);});case 111:return _45(_5y,function(_5G){return A(_5B,[[5,[0,_5y,_5G]]]);});case 120:return _45(_5z,function(_5H){return A(_5B,[[5,[0,_5z,_5H]]]);});default:return [2];}}]):[2];}];},_5I=false,_5J=true,_5K=function(_5L){return [0,function(_5M){switch(E(E(_5M)[1])){case 79:return A(_5L,[_5y]);case 88:return A(_5L,[_5z]);case 111:return A(_5L,[_5y]);case 120:return A(_5L,[_5z]);default:return [2];}}];},_5N=function(_5O){return A(_5O,[_4v]);},_5P=function(_5Q){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _o(9,_5Q,_t);})));},_5R=function(_5S){var _5T=E(_5S);return _5T[0]==0?E(_5T[1]):I_toInt(_5T[1]);},_5U=function(_5V,_5W){var _5X=E(_5V);if(!_5X[0]){var _5Y=_5X[1],_5Z=E(_5W);return _5Z[0]==0?_5Y<=_5Z[1]:I_compareInt(_5Z[1],_5Y)>=0;}else{var _60=_5X[1],_61=E(_5W);return _61[0]==0?I_compareInt(_60,_61[1])<=0:I_compare(_60,_61[1])<=0;}},_62=function(_63){return [2];},_64=function(_65){var _66=E(_65);if(!_66[0]){return E(_62);}else{var _67=_66[1],_68=E(_66[2]);return _68[0]==0?E(_67):function(_69){return _2y(A(_67,[_69]),new T(function(){return A(_64(_68),[_69]);}));};}},_6a=unCStr("NUL"),_6b=function(_6c){return [2];},_6d=function(_6e){return _6b(_6e);},_6f=function(_6g,_6h){var _6i=function(_6j,_6k){var _6l=E(_6j);if(!_6l[0]){return function(_6m){return A(_6m,[_6g]);};}else{var _6n=E(_6k);return _6n[0]==0?E(_6b):E(_6l[1])[1]!=E(_6n[1])[1]?E(_6d):function(_6o){return [0,function(_6p){return A(_6i(_6l[2],_6n[2]),[_6o]);}];};}};return [1,function(_6q){return A(_6i,[_6g,_6q,_6h]);}];},_6r=[0,0],_6s=function(_6t){return _6f(_6a,function(_6u){return A(_6t,[_6r]);});},_6v=unCStr("STX"),_6w=[0,2],_6x=function(_6y){return _6f(_6v,function(_6z){return A(_6y,[_6w]);});},_6A=unCStr("ETX"),_6B=[0,3],_6C=function(_6D){return _6f(_6A,function(_6E){return A(_6D,[_6B]);});},_6F=unCStr("EOT"),_6G=[0,4],_6H=function(_6I){return _6f(_6F,function(_6J){return A(_6I,[_6G]);});},_6K=unCStr("ENQ"),_6L=[0,5],_6M=function(_6N){return _6f(_6K,function(_6O){return A(_6N,[_6L]);});},_6P=unCStr("ACK"),_6Q=[0,6],_6R=function(_6S){return _6f(_6P,function(_6T){return A(_6S,[_6Q]);});},_6U=unCStr("BEL"),_6V=[0,7],_6W=function(_6X){return _6f(_6U,function(_6Y){return A(_6X,[_6V]);});},_6Z=unCStr("BS"),_70=[0,8],_71=function(_72){return _6f(_6Z,function(_73){return A(_72,[_70]);});},_74=unCStr("HT"),_75=[0,9],_76=function(_77){return _6f(_74,function(_78){return A(_77,[_75]);});},_79=unCStr("LF"),_7a=[0,10],_7b=function(_7c){return _6f(_79,function(_7d){return A(_7c,[_7a]);});},_7e=unCStr("VT"),_7f=[0,11],_7g=function(_7h){return _6f(_7e,function(_7i){return A(_7h,[_7f]);});},_7j=unCStr("FF"),_7k=[0,12],_7l=function(_7m){return _6f(_7j,function(_7n){return A(_7m,[_7k]);});},_7o=unCStr("CR"),_7p=[0,13],_7q=function(_7r){return _6f(_7o,function(_7s){return A(_7r,[_7p]);});},_7t=unCStr("SI"),_7u=[0,15],_7v=function(_7w){return _6f(_7t,function(_7x){return A(_7w,[_7u]);});},_7y=unCStr("DLE"),_7z=[0,16],_7A=function(_7B){return _6f(_7y,function(_7C){return A(_7B,[_7z]);});},_7D=unCStr("DC1"),_7E=[0,17],_7F=function(_7G){return _6f(_7D,function(_7H){return A(_7G,[_7E]);});},_7I=unCStr("DC2"),_7J=[0,18],_7K=function(_7L){return _6f(_7I,function(_7M){return A(_7L,[_7J]);});},_7N=unCStr("DC3"),_7O=[0,19],_7P=function(_7Q){return _6f(_7N,function(_7R){return A(_7Q,[_7O]);});},_7S=unCStr("DC4"),_7T=[0,20],_7U=function(_7V){return _6f(_7S,function(_7W){return A(_7V,[_7T]);});},_7X=unCStr("NAK"),_7Y=[0,21],_7Z=function(_80){return _6f(_7X,function(_81){return A(_80,[_7Y]);});},_82=unCStr("SYN"),_83=[0,22],_84=function(_85){return _6f(_82,function(_86){return A(_85,[_83]);});},_87=unCStr("ETB"),_88=[0,23],_89=function(_8a){return _6f(_87,function(_8b){return A(_8a,[_88]);});},_8c=unCStr("CAN"),_8d=[0,24],_8e=function(_8f){return _6f(_8c,function(_8g){return A(_8f,[_8d]);});},_8h=unCStr("EM"),_8i=[0,25],_8j=function(_8k){return _6f(_8h,function(_8l){return A(_8k,[_8i]);});},_8m=unCStr("SUB"),_8n=[0,26],_8o=function(_8p){return _6f(_8m,function(_8q){return A(_8p,[_8n]);});},_8r=unCStr("ESC"),_8s=[0,27],_8t=function(_8u){return _6f(_8r,function(_8v){return A(_8u,[_8s]);});},_8w=unCStr("FS"),_8x=[0,28],_8y=function(_8z){return _6f(_8w,function(_8A){return A(_8z,[_8x]);});},_8B=unCStr("GS"),_8C=[0,29],_8D=function(_8E){return _6f(_8B,function(_8F){return A(_8E,[_8C]);});},_8G=unCStr("RS"),_8H=[0,30],_8I=function(_8J){return _6f(_8G,function(_8K){return A(_8J,[_8H]);});},_8L=unCStr("US"),_8M=[0,31],_8N=function(_8O){return _6f(_8L,function(_8P){return A(_8O,[_8M]);});},_8Q=unCStr("SP"),_8R=[0,32],_8S=function(_8T){return _6f(_8Q,function(_8U){return A(_8T,[_8R]);});},_8V=unCStr("DEL"),_8W=[0,127],_8X=function(_8Y){return _6f(_8V,function(_8Z){return A(_8Y,[_8W]);});},_90=[1,_8X,_t],_91=[1,_8S,_90],_92=[1,_8N,_91],_93=[1,_8I,_92],_94=[1,_8D,_93],_95=[1,_8y,_94],_96=[1,_8t,_95],_97=[1,_8o,_96],_98=[1,_8j,_97],_99=[1,_8e,_98],_9a=[1,_89,_99],_9b=[1,_84,_9a],_9c=[1,_7Z,_9b],_9d=[1,_7U,_9c],_9e=[1,_7P,_9d],_9f=[1,_7K,_9e],_9g=[1,_7F,_9f],_9h=[1,_7A,_9g],_9i=[1,_7v,_9h],_9j=[1,_7q,_9i],_9k=[1,_7l,_9j],_9l=[1,_7g,_9k],_9m=[1,_7b,_9l],_9n=[1,_76,_9m],_9o=[1,_71,_9n],_9p=[1,_6W,_9o],_9q=[1,_6R,_9p],_9r=[1,_6M,_9q],_9s=[1,_6H,_9r],_9t=[1,_6C,_9s],_9u=[1,_6x,_9t],_9v=[1,_6s,_9u],_9w=unCStr("SOH"),_9x=[0,1],_9y=function(_9z){return _6f(_9w,function(_9A){return A(_9z,[_9x]);});},_9B=unCStr("SO"),_9C=[0,14],_9D=function(_9E){return _6f(_9B,function(_9F){return A(_9E,[_9C]);});},_9G=function(_9H){return _3G(_9y,_9D,_9H);},_9I=[1,_9G,_9v],_9J=new T(function(){return _64(_9I);}),_9K=[0,1114111],_9L=[0,34],_9M=[0,_9L,_5J],_9N=[0,39],_9O=[0,_9N,_5J],_9P=[0,92],_9Q=[0,_9P,_5J],_9R=[0,_6V,_5J],_9S=[0,_70,_5J],_9T=[0,_7k,_5J],_9U=[0,_7a,_5J],_9V=[0,_7p,_5J],_9W=[0,_75,_5J],_9X=[0,_7f,_5J],_9Y=[0,_6r,_5J],_9Z=[0,_9x,_5J],_a0=[0,_6w,_5J],_a1=[0,_6B,_5J],_a2=[0,_6G,_5J],_a3=[0,_6L,_5J],_a4=[0,_6Q,_5J],_a5=[0,_6V,_5J],_a6=[0,_70,_5J],_a7=[0,_75,_5J],_a8=[0,_7a,_5J],_a9=[0,_7f,_5J],_aa=[0,_7k,_5J],_ab=[0,_7p,_5J],_ac=[0,_9C,_5J],_ad=[0,_7u,_5J],_ae=[0,_7z,_5J],_af=[0,_7E,_5J],_ag=[0,_7J,_5J],_ah=[0,_7O,_5J],_ai=[0,_7T,_5J],_aj=[0,_7Y,_5J],_ak=[0,_83,_5J],_al=[0,_88,_5J],_am=[0,_8d,_5J],_an=[0,_8i,_5J],_ao=[0,_8n,_5J],_ap=[0,_8s,_5J],_aq=[0,_8x,_5J],_ar=[0,_8C,_5J],_as=[0,_8H,_5J],_at=[0,_8M,_5J],_au=function(_av){return [0,_av];},_aw=function(_ax){return _2y([0,function(_ay){switch(E(E(_ay)[1])){case 34:return A(_ax,[_9M]);case 39:return A(_ax,[_9O]);case 92:return A(_ax,[_9Q]);case 97:return A(_ax,[_9R]);case 98:return A(_ax,[_9S]);case 102:return A(_ax,[_9T]);case 110:return A(_ax,[_9U]);case 114:return A(_ax,[_9V]);case 116:return A(_ax,[_9W]);case 118:return A(_ax,[_9X]);default:return [2];}}],new T(function(){return _2y(_3G(_5K,_5N,function(_az){return _45(_az,function(_aA){var _aB=_4W(new T(function(){return _au(E(_az)[1]);}),_4N,_aA);return !_5U(_aB,_9K)?[2]:A(_ax,[[0,new T(function(){var _aC=_5R(_aB);return _aC>>>0>1114111?_5P(_aC):[0,_aC];}),_5J]]);});}),new T(function(){return _2y([0,function(_aD){return E(E(_aD)[1])==94?E([0,function(_aE){switch(E(E(_aE)[1])){case 64:return A(_ax,[_9Y]);case 65:return A(_ax,[_9Z]);case 66:return A(_ax,[_a0]);case 67:return A(_ax,[_a1]);case 68:return A(_ax,[_a2]);case 69:return A(_ax,[_a3]);case 70:return A(_ax,[_a4]);case 71:return A(_ax,[_a5]);case 72:return A(_ax,[_a6]);case 73:return A(_ax,[_a7]);case 74:return A(_ax,[_a8]);case 75:return A(_ax,[_a9]);case 76:return A(_ax,[_aa]);case 77:return A(_ax,[_ab]);case 78:return A(_ax,[_ac]);case 79:return A(_ax,[_ad]);case 80:return A(_ax,[_ae]);case 81:return A(_ax,[_af]);case 82:return A(_ax,[_ag]);case 83:return A(_ax,[_ah]);case 84:return A(_ax,[_ai]);case 85:return A(_ax,[_aj]);case 86:return A(_ax,[_ak]);case 87:return A(_ax,[_al]);case 88:return A(_ax,[_am]);case 89:return A(_ax,[_an]);case 90:return A(_ax,[_ao]);case 91:return A(_ax,[_ap]);case 92:return A(_ax,[_aq]);case 93:return A(_ax,[_ar]);case 94:return A(_ax,[_as]);case 95:return A(_ax,[_at]);default:return [2];}}]):[2];}],new T(function(){return A(_9J,[function(_aF){return A(_ax,[[0,_aF,_5J]]);}]);}));}));}));},_aG=function(_aH){return A(_aH,[_0]);},_aI=function(_aJ){var _aK=E(_aJ);if(!_aK[0]){return E(_aG);}else{var _aL=_aK[2],_aM=E(E(_aK[1])[1]);switch(_aM){case 9:return function(_aN){return [0,function(_aO){return A(_aI(_aL),[_aN]);}];};case 10:return function(_aP){return [0,function(_aQ){return A(_aI(_aL),[_aP]);}];};case 11:return function(_aR){return [0,function(_aS){return A(_aI(_aL),[_aR]);}];};case 12:return function(_aT){return [0,function(_aU){return A(_aI(_aL),[_aT]);}];};case 13:return function(_aV){return [0,function(_aW){return A(_aI(_aL),[_aV]);}];};case 32:return function(_aX){return [0,function(_aY){return A(_aI(_aL),[_aX]);}];};case 160:return function(_aZ){return [0,function(_b0){return A(_aI(_aL),[_aZ]);}];};default:var _b1=u_iswspace(_aM);return E(_b1)==0?E(_aG):function(_b2){return [0,function(_b3){return A(_aI(_aL),[_b2]);}];};}}},_b4=function(_b5){var _b6=new T(function(){return _b4(_b5);}),_b7=[1,function(_b8){return A(_aI,[_b8,function(_b9){return E([0,function(_ba){return E(E(_ba)[1])==92?E(_b6):[2];}]);}]);}];return _2y([0,function(_bb){return E(E(_bb)[1])==92?E([0,function(_bc){var _bd=E(E(_bc)[1]);switch(_bd){case 9:return E(_b7);case 10:return E(_b7);case 11:return E(_b7);case 12:return E(_b7);case 13:return E(_b7);case 32:return E(_b7);case 38:return E(_b6);case 160:return E(_b7);default:var _be=u_iswspace(_bd);return E(_be)==0?[2]:E(_b7);}}]):[2];}],[0,function(_bf){var _bg=E(_bf);return E(_bg[1])==92?_aw(_b5):A(_b5,[[0,_bg,_5I]]);}]);},_bh=function(_bi,_bj){return _b4(function(_bk){var _bl=E(_bk),_bm=E(_bl[1]);return E(_bm[1])==34?!E(_bl[2])?A(_bj,[[1,new T(function(){return A(_bi,[_t]);})]]):_bh(function(_bn){return A(_bi,[[1,_bm,_bn]]);},_bj):_bh(function(_bo){return A(_bi,[[1,_bm,_bo]]);},_bj);});},_bp=unCStr("_\'"),_bq=function(_br){var _bs=u_iswalnum(_br);return E(_bs)==0?_5q(_35,[0,_br],_bp):true;},_bt=function(_bu){return _bq(E(_bu)[1]);},_bv=unCStr(",;()[]{}`"),_bw=function(_bx){return A(_bx,[_t]);},_by=function(_bz,_bA){var _bB=function(_bC){var _bD=E(_bC);if(!_bD[0]){return E(_bw);}else{var _bE=_bD[1];return !A(_bz,[_bE])?E(_bw):function(_bF){return [0,function(_bG){return A(_bB(_bD[2]),[function(_bH){return A(_bF,[[1,_bE,_bH]]);}]);}];};}};return [1,function(_bI){return A(_bB,[_bI,_bA]);}];},_bJ=unCStr(".."),_bK=unCStr("::"),_bL=unCStr("->"),_bM=[0,64],_bN=[1,_bM,_t],_bO=[0,126],_bP=[1,_bO,_t],_bQ=unCStr("=>"),_bR=[1,_bQ,_t],_bS=[1,_bP,_bR],_bT=[1,_bN,_bS],_bU=[1,_bL,_bT],_bV=unCStr("<-"),_bW=[1,_bV,_bU],_bX=[0,124],_bY=[1,_bX,_t],_bZ=[1,_bY,_bW],_c0=[1,_9P,_t],_c1=[1,_c0,_bZ],_c2=[0,61],_c3=[1,_c2,_t],_c4=[1,_c3,_c1],_c5=[1,_bK,_c4],_c6=[1,_bJ,_c5],_c7=function(_c8){return _2y([1,function(_c9){return E(_c9)[0]==0?A(_c8,[_40]):[2];}],new T(function(){return _2y([0,function(_ca){return E(E(_ca)[1])==39?E([0,function(_cb){var _cc=E(_cb);switch(E(_cc[1])){case 39:return [2];case 92:return _aw(function(_cd){var _ce=E(_cd);return (function(_cf,_cg){var _ch=new T(function(){return A(_c8,[[0,_cf]]);});return !E(_cg)?E(E(_cf)[1])==39?[2]:[0,function(_ci){return E(E(_ci)[1])==39?E(_ch):[2];}]:[0,function(_cj){return E(E(_cj)[1])==39?E(_ch):[2];}];})(_ce[1],_ce[2]);});default:return [0,function(_ck){return E(E(_ck)[1])==39?A(_c8,[[0,_cc]]):[2];}];}}]):[2];}],new T(function(){return _2y([0,function(_cl){return E(E(_cl)[1])==34?_bh(_41,_c8):[2];}],new T(function(){return _2y([0,function(_cm){return !_5q(_35,_cm,_bv)?[2]:A(_c8,[[2,[1,_cm,_t]]]);}],new T(function(){return _2y([0,function(_cn){return !_5q(_35,_cn,_5v)?[2]:_by(_5w,function(_co){var _cp=[1,_cn,_co];return !_5q(_3m,_cp,_c6)?A(_c8,[[4,_cp]]):A(_c8,[[2,_cp]]);});}],new T(function(){return _2y([0,function(_cq){var _cr=E(_cq),_cs=_cr[1],_ct=u_iswalpha(_cs);return E(_ct)==0?E(_cs)==95?_by(_bt,function(_cu){return A(_c8,[[3,[1,_cr,_cu]]]);}):[2]:_by(_bt,function(_cv){return A(_c8,[[3,[1,_cr,_cv]]]);});}],new T(function(){return _3G(_5A,_5l,_c8);}));}));}));}));}));}));},_cw=function(_cx){return [1,function(_cy){return A(_aI,[_cy,function(_cz){return _c7(_cx);}]);}];},_cA=[0,0],_cB=function(_cC,_cD){return _cw(function(_cE){var _cF=E(_cE);if(_cF[0]==2){var _cG=E(_cF[1]);return _cG[0]==0?[2]:E(E(_cG[1])[1])==40?E(_cG[2])[0]==0?A(_cC,[_cA,function(_cH){return _cw(function(_cI){var _cJ=E(_cI);if(_cJ[0]==2){var _cK=E(_cJ[1]);return _cK[0]==0?[2]:E(E(_cK[1])[1])==41?E(_cK[2])[0]==0?A(_cD,[_cH]):[2]:[2];}else{return [2];}});}]):[2]:[2];}else{return [2];}});},_cL=function(_cM,_cN,_cO){var _cP=function(_cQ,_cR){return _2y(_cw(function(_cS){var _cT=E(_cS);if(_cT[0]==4){var _cU=E(_cT[1]);return _cU[0]==0?A(_cM,[_cT,_cQ,_cR]):E(E(_cU[1])[1])==45?E(_cU[2])[0]==0?E([1,function(_cV){return A(_aI,[_cV,function(_cW){return _c7(function(_cX){return A(_cM,[_cX,_cQ,function(_cY){return A(_cR,[new T(function(){return [0, -E(_cY)[1]];})]);}]);});}]);}]):A(_cM,[_cT,_cQ,_cR]):A(_cM,[_cT,_cQ,_cR]);}else{return A(_cM,[_cT,_cQ,_cR]);}}),new T(function(){return _cB(_cP,_cR);}));};return _cP(_cN,_cO);},_cZ=function(_d0,_d1){return [2];},_d2=function(_d3,_d4){return _cZ(_d3,_d4);},_d5=function(_d6){var _d7=E(_d6);return _d7[0]==0?[1,new T(function(){return _4W(new T(function(){return _au(E(_d7[1])[1]);}),_4N,_d7[2]);})]:E(_d7[2])[0]==0?E(_d7[3])[0]==0?[1,new T(function(){return _4W(_4M,_4N,_d7[1]);})]:[0]:[0];},_d8=function(_d9){var _da=E(_d9);if(_da[0]==5){var _db=_d5(_da[1]);return _db[0]==0?E(_cZ):function(_dc,_dd){return A(_dd,[new T(function(){return [0,_5R(_db[1])];})]);};}else{return E(_d2);}},_de=function(_df){return [1,function(_dg){return A(_aI,[_dg,function(_dh){return E([3,_df,_3y]);}]);}];},_di=new T(function(){return _cL(_d8,_cA,_de);}),_dj=function(_dk){return E(E(_dk)[1])==45?true:false;},_dl=function(_dm){while(1){var _dn=(function(_do){var _dp=E(_do);if(!_dp[0]){return [0];}else{var _dq=_dp[2],_dr=E(_dp[1]);if(!E(_dr[2])[0]){return [1,_dr[1],new T(function(){return _dl(_dq);})];}else{_dm=_dq;return null;}}})(_dm);if(_dn!=null){return _dn;}}},_ds=function(_dt,_du){var _dv=E(_du);if(!_dv[0]){return [0,_t,_t];}else{var _dw=_dv[1];if(!A(_dt,[_dw])){var _dx=new T(function(){var _dy=_ds(_dt,_dv[2]);return [0,_dy[1],_dy[2]];});return [0,[1,_dw,new T(function(){return E(E(_dx)[1]);})],new T(function(){return E(E(_dx)[2]);})];}else{return [0,_t,_dv];}}},_dz=function(_dA,_dB){while(1){var _dC=E(_dB);if(!_dC[0]){return [0];}else{if(!A(_dA,[_dC[1]])){return E(_dC);}else{_dB=_dC[2];continue;}}}},_dD=function(_dE,_dF){var _dG=_dz(_dE,_dF);if(!_dG[0]){return [0];}else{var _dH=new T(function(){var _dI=_ds(_dE,_dG);return [0,_dI[1],_dI[2]];});return [1,new T(function(){return E(E(_dH)[1]);}),new T(function(){return _dD(_dE,E(_dH)[2]);})];}},_dJ=function(_dK){var _dL=new T(function(){return _dD(_dj,_dK);});return [0,new T(function(){var _dM=_dl(_2o(_di,new T(function(){return _Z(_dL,0);})));return _dM[0]==0?E(_17):E(_dM[2])[0]==0?E(_dM[1]):E(_15);}),new T(function(){var _dN=_dl(_2o(_di,new T(function(){return _Z(_dL,1);})));return _dN[0]==0?E(_17):E(_dN[2])[0]==0?E(_dN[1]):E(_15);})];},_dO=function(_dP){return E(E(_dP)[1])==124?true:false;},_dQ=unCStr("horizontal"),_dR=function(_dS,_dT){while(1){var _dU=E(_dS);if(!_dU[0]){return E(_dT)[0]==0?true:false;}else{var _dV=E(_dT);if(!_dV[0]){return false;}else{if(E(_dU[1])[1]!=E(_dV[1])[1]){return false;}else{_dS=_dU[2];_dT=_dV[2];continue;}}}}},_dW=function(_dX){return fromJSStr(E(_dX)[1]);},_dY=unCStr("submarine"),_dZ=unCStr("destroyer"),_e0=unCStr("battleship"),_e1=unCStr("aircraftcarrier"),_e2=function(_e3){return !_dR(_e3,_e1)?!_dR(_e3,_e0)?!_dR(_e3,_dZ)?!_dR(_e3,_dY)?4:2:3:1:0;},_e4=function(_e5){var _e6=new T(function(){return _dD(_dO,_dW(_e5));});return [0,new T(function(){return _e2(_Z(_e6,0));}),new T(function(){var _e7=_dJ(new T(function(){return _Z(_e6,1);}));return [0,_e7[1],_e7[2]];}),new T(function(){return !_dR(_Z(_e6,2),_dQ)?0:1;})];},_e8=function(_e9,_ea,_eb,_ec){if(!E(_ec)){var _ed=function(_ee){var _ef=_ee-1|0;if(0<=_ef){var _eg=function(_eh){return [1,[0,new T(function(){return [0,E(_ea)[1]+_eh|0];}),_eb],new T(function(){return _eh!=_ef?_eg(_eh+1|0):[0];})];};return _eg(0);}else{return [0];}};switch(E(_e9)){case 0:return _ed(5);case 1:return _ed(4);case 2:return _ed(3);case 3:return _ed(3);default:return _ed(2);}}else{var _ei=function(_ej){var _ek=_ej-1|0;if(0<=_ek){var _el=function(_em){return [1,[0,_ea,new T(function(){return [0,E(_eb)[1]+_em|0];})],new T(function(){return _em!=_ek?_el(_em+1|0):[0];})];};return _el(0);}else{return [0];}};switch(E(_e9)){case 0:return _ei(5);case 1:return _ei(4);case 2:return _ei(3);case 3:return _ei(3);default:return _ei(2);}}},_en=function(_eo){var _ep=E(_eo);if(!_ep[0]){return [0];}else{var _eq=E(_ep[1]),_er=E(_eq[2]);return _e(_e8(_eq[1],_er[1],_er[2],_eq[3]),new T(function(){return _en(_ep[2]);}));}},_es=function(_et,_eu){switch(E(_et)){case 0:switch(E(_eu)){case 0:return false;case 1:return true;case 2:return true;case 3:return true;default:return true;}break;case 1:return E(_eu)==1?false:true;case 2:return E(_eu)==2?false:true;case 3:return E(_eu)==3?false:true;default:return E(_eu)==4?false:true;}},_ev=function(_ew,_ex){switch(E(_ew)){case 0:switch(E(_ex)){case 0:return true;case 1:return false;case 2:return false;case 3:return false;default:return false;}break;case 1:return E(_ex)==1?true:false;case 2:return E(_ex)==2?true:false;case 3:return E(_ex)==3?true:false;default:return E(_ex)==4?true:false;}},_ey=[0,_ev,_es],_ez=function(_eA,_eB){while(1){var _eC=E(_eA);if(!_eC[0]){return E(_eB);}else{_eA=_eC[2];var _eD=_eB+1|0;_eB=_eD;continue;}}},_eE=0,_eF=1,_eG=3,_eH=2,_eI=function(_eJ,_eK){var _eL=function(_eM,_eN){while(1){var _eO=(function(_eP,_eQ){var _eR=E(_eQ);if(!_eR[0]){return [0];}else{var _eS=_eR[2];if(!A(_eJ,[_eR[1]])){var _eT=_eP+1|0;_eN=_eS;_eM=_eT;return null;}else{return [1,[0,_eP],new T(function(){return _eL(_eP+1|0,_eS);})];}}})(_eM,_eN);if(_eO!=null){return _eO;}}};return _eL(0,_eK);},_eU=function(_eV){return E(_eV)==4?true:false;},_eW=function(_eX){return E(E(_eX)[1]);},_eY=function(_eZ){var _f0=E(_eZ);return _f0[0]==0?[0]:[1,new T(function(){return _eW(_f0[1]);}),new T(function(){return _eY(_f0[2]);})];},_f1=function(_f2,_f3){switch(E(_f3)){case 0:return !_5q(_ey,_eE,_eY(_f2))?true:false;case 1:return !_5q(_ey,_eF,_eY(_f2))?true:false;case 2:return !_5q(_ey,_eH,_eY(_f2))?true:false;case 3:return !_5q(_ey,_eG,_eY(_f2))?true:false;default:return _ez(_eI(_eU,_eY(_f2)),0)<2;}},_f4=function(_f5,_f6){while(1){var _f7=E(_f6);if(!_f7[0]){return false;}else{if(!A(_f5,[_f7[1]])){_f6=_f7[2];continue;}else{return true;}}}},_f8=function(_f9){var _fa=E(_f9),_fb=E(_fa[2]);return _e8(_fa[1],_fb[1],_fb[2],_fa[3]);},_fc=function(_fd,_fe){return E(_fd)[1]==E(_fe)[1];},_ff=function(_fg,_fh){return E(_fg)[1]!=E(_fh)[1];},_fi=[0,_fc,_ff],_fj=function(_fk,_fl){if(_fk<=_fl){var _fm=function(_fn){return [1,[0,_fn],new T(function(){return _fn!=_fl?_fm(_fn+1|0):[0];})];};return _fm(_fk);}else{return [0];}},_fo=new T(function(){return _fj(0,9);}),_fp=function(_fq,_fr,_fs){return !_5q(_fi,_fq,_fo)?false:!_5q(_fi,_fr,_fo)?false:E(_fs);},_ft=function(_fu){var _fv=E(_fu);if(!_fv[0]){return true;}else{var _fw=E(_fv[1]);return _fp(_fw[1],_fw[2],new T(function(){return _ft(_fv[2]);}));}},_fx=function(_fy,_fz,_fA,_fB,_fC,_fD){return !A(_fy,[_fA,_fC])?true:!A(_36,[_fz,_fB,_fD])?true:false;},_fE=function(_fF,_fG,_fH,_fI){var _fJ=E(_fH),_fK=E(_fI);return _fx(E(_fF)[1],_fG,_fJ[1],_fJ[2],_fK[1],_fK[2]);},_fL=function(_fM,_fN,_fO,_fP,_fQ,_fR){return !A(_fM,[_fO,_fQ])?false:A(_36,[_fN,_fP,_fR]);},_fS=function(_fT,_fU,_fV,_fW){var _fX=E(_fV),_fY=E(_fW);return _fL(E(_fT)[1],_fU,_fX[1],_fX[2],_fY[1],_fY[2]);},_fZ=function(_g0,_g1){return [0,function(_3k,_3l){return _fS(_g0,_g1,_3k,_3l);},function(_3k,_3l){return _fE(_g0,_g1,_3k,_3l);}];},_g2=new T(function(){return _fZ(_fi,_fi);}),_g3=new T(function(){return _38(_g2,_t,_t);}),_g4=function(_g5,_g6){var _g7=new T(function(){return !_ft(_f8(_g6))?false:_f1(_g5,E(_g6)[1]);}),_g8=_en(_g5);if(!_g8[0]){return !E(_g3)?false:E(_g7);}else{var _g9=E(_g6),_ga=E(_g9[2]),_gb=_e8(_g9[1],_ga[1],_ga[2],_g9[3]);if(!_gb[0]){return !E(_g3)?false:E(_g7);}else{var _gc=function(_gd){while(1){var _ge=(function(_gf){var _gg=E(_gf);if(!_gg[0]){return [0];}else{var _gh=_gg[1],_gi=_gg[2];if(!_f4(function(_gj){var _gk=E(_gh),_gl=E(_gj);return E(_gk[1])[1]!=E(_gl[1])[1]?false:_fc(_gk[2],_gl[2]);},_gb)){_gd=_gi;return null;}else{return [1,_gh,new T(function(){return _gc(_gi);})];}}})(_gd);if(_ge!=null){return _ge;}}};return !_38(_g2,(function(_gm,_gn){return !_f4(function(_go){var _gp=E(_gm),_gq=E(_go);return E(_gp[1])[1]!=E(_gq[1])[1]?false:_fc(_gp[2],_gq[2]);},_gb)?_gc(_gn):[1,_gm,new T(function(){return _gc(_gn);})];})(_g8[1],_g8[2]),_t)?false:E(_g7);}}},_gr=unCStr("Invalid position for boat!"),_gs=[0,46],_gt=[1,_gs,_t],_gu=function(_gv,_gw){while(1){var _gx=E(_gv);if(!_gx){return E(_gw);}else{var _gy=E(_gw);if(!_gy[0]){return [0];}else{_gv=_gx-1|0;_gw=_gy[2];continue;}}}},_gz=function(_gA,_gB,_gC,_){var _gD=rMV(_gC),_gE=new T(function(){var _gF=_e4(_gB);return [0,_gF[1],new T(function(){var _gG=_dJ(new T(function(){return _gu(1,fromJSStr(E(_gA)[1]));}));return [0,_gG[1],_gG[2]];}),_gF[3]];});debug(toJSStr(unAppCStr("Trying to add ",new T(function(){var _gH=E(_gE);return _e(_L(0,_gH[1],_gH[2],_gH[3],_t),_gt);}))));if(!_g4(E(E(_gD)[1])[1],_gE)){message(toJSStr(E(_gr)));return _0;}else{addBoat(E(_gA)[1]);return _0;}},_gI=function(_gJ,_gK,_gL,_){return _gz(_gJ,_gK,E(_gL)[1],_);},_gM=unCStr("Boat can not be flipped at position!"),_gN=[1,_gs,_t],_gO=function(_gP,_gQ,_gR,_){var _gS=rMV(_gR),_gT=new T(function(){var _gU=_e4(_gQ);return [0,_gU[1],_gU[2],_gU[3]];});debug(toJSStr(unAppCStr("Trying to flip ",new T(function(){var _gV=E(_gT);return _e(_L(0,_gV[1],_gV[2],_gV[3],_t),_gN);}))));if(!_g4(E(E(_gS)[1])[1],new T(function(){var _gW=E(_gT);return [0,_gW[1],_gW[2],new T(function(){return E(_gW[3])==0?1:0;})];}))){message(toJSStr(E(_gM)));return _0;}else{flipBoat(E(_gP)[1]);return _0;}},_gX=function(_gY,_gZ,_h0,_){return _gO(_gY,_gZ,E(_h0)[1],_);},_h1=unCStr("boats = "),_h2=unCStr("Fleet {"),_h3=function(_h4,_h5,_h6){var _h7=function(_h8){return _e(_h2,new T(function(){return _e(_h1,new T(function(){var _h9=[1,_8,_h8],_ha=E(_h5);return _ha[0]==0?unAppCStr("[]",_h9):[1,_1C,new T(function(){var _hb=E(_ha[1]);return _L(0,_hb[1],_hb[2],_hb[3],new T(function(){var _hc=function(_hd){var _he=E(_hd);return _he[0]==0?E([1,_1B,_h9]):[1,_1,new T(function(){var _hf=E(_he[1]);return _L(0,_hf[1],_hf[2],_hf[3],new T(function(){return _hc(_he[2]);}));})];};return _hc(_ha[2]);}));})];}));}));};return _h4<11?_h7(_h6):[1,_n,new T(function(){return _h7([1,_m,_h6]);})];},_hg=[1,_gs,_t],_hh=unCStr("Boat could not be added to fleet!"),_hi=unCStr("Boat locked, go on!"),_hj=unCStr(" locked!"),_hk=function(_hl,_hm,_hn,_){var _ho=rMV(_hn),_hp=E(E(_ho)[1])[1],_hq=new T(function(){var _hr=_e4(_hm);return [0,_hr[1],_hr[2],_hr[3]];});if(!_g4(_hp,_hq)){message(toJSStr(E(_hh)));return _0;}else{var _hs=new T(function(){return _e(_hp,[1,_hq,_t]);});wMV(_hn,[0,[0,_hs]]);var _ht=E(_hq);debug(toJSStr(_e(_L(0,_ht[1],_ht[2],_ht[3],_t),_hj)));debug(toJSStr(unAppCStr("New ",new T(function(){return _e(_h3(0,_hs,_t),_hg);}))));message(toJSStr(E(_hi)));lockBoat(E(_hl)[1]);return _0;}},_hu=function(_hv,_hw,_hx,_){return _hk(_hv,_hw,E(_hx)[1],_);},_hy=unCStr("button#lock"),_hz=unCStr("button#start"),_hA=function(_hB,_hC,_){resetGame(E(_hB)[1]);return _0;},_hD=[0,_hA],_hE=unCStr("button#reset"),_hF=function(_hG,_hH,_){selectBoat(E(_hG)[1]);return _0;},_hI=[0,_hF],_hJ=unCStr(".boat"),_hK=unCStr("tbody td:not(.shead)"),_hL=unCStr("button#flip"),_hM=function(_hN){return E(_hN)==3?true:false;},_hO=function(_hP){return E(_hP)==2?true:false;},_hQ=function(_hR){return E(_hR)==1?true:false;},_hS=function(_hT){switch(E(_hT)){case 0:return true;case 1:return false;case 2:return false;case 3:return false;default:return false;}},_hU=function(_hV){var _hW=E(_hV);return _hW[0]==0?[0]:[1,new T(function(){return _eW(_hW[1]);}),new T(function(){return _hU(_hW[2]);})];},_hX=function(_hY){var _hZ=E(_hY);if(!_hZ[0]){return [0];}else{var _i0=E(_hZ[1]),_i1=E(_i0[2]);return _e(_e8(_i0[1],_i1[1],_i1[2],_i0[3]),new T(function(){return _hX(_hZ[2]);}));}},_i2=function(_i3,_i4){var _i5=function(_i6,_i7){while(1){var _i8=(function(_i9,_ia){var _ib=E(_i9);if(!_ib[0]){return [0];}else{var _ic=_ib[1],_id=_ib[2];if(!_5q(_i3,_ic,_ia)){return [1,_ic,new T(function(){return _i5(_id,[1,_ic,_ia]);})];}else{_i6=_id;var _ie=_ia;_i7=_ie;return null;}}})(_i6,_i7);if(_i8!=null){return _i8;}}};return _i5(_i4,_t);},_if=function(_ig){if(_ez(_ig,0)==6){var _ih=_hX(_ig);if(!_38(_g2,_i2(_g2,_ih),_ih)){return false;}else{var _ii=_hU(_ig);return _ez(_eI(_hS,_ii),0)==1?_ez(_eI(_hQ,_ii),0)==1?_ez(_eI(_hO,_ii),0)==1?_ez(_eI(_hM,_ii),0)==1?_ez(_eI(_eU,_ii),0)==2?true:false:false:false:false:false;}}else{return false;}},_ij=[1,_5c,_t],_ik=function(_il){return _il>1?[1,_5c,new T(function(){return _ik(_il-1|0);})]:E(_ij);},_im=new T(function(){return _ik(10);}),_in=[1,_im,_t],_io=function(_ip){return _ip>1?[1,_im,new T(function(){return _io(_ip-1|0);})]:E(_in);},_iq=new T(function(){return _io(10);}),_ir=[0,_iq],_is=unCStr("Prelude.(!!): negative index\n"),_it=new T(function(){return err(_is);}),_iu=function(_iv){return E(_iv)[0]==0?false:true;},_iw=function(_ix,_iy){var _iz=E(_iy);return _iz[0]==0?[0]:[1,new T(function(){return A(_ix,[_iz[1]]);}),new T(function(){return _iw(_ix,_iz[2]);})];},_iA=function(_iB,_iC){var _iD=_eI(_iu,_iw(function(_iE){var _iF=_eI(function(_iG){var _iH=E(_iG),_iI=E(_iB);return E(_iI[1])[1]!=E(_iH[1])[1]?false:_fc(_iI[2],_iH[2]);},_f8(_iE));return _iF[0]==0?[0]:[1,_iF[1]];},_iC));return _iD[0]==0?[0]:[1,new T(function(){var _iJ=E(_iD[1])[1];return _iJ>=0?_Z(_iC,_iJ):E(_it);})];},_iK=function(_iL,_iM,_iN){var _iO=E(_iN);if(!_iO[0]){return [0];}else{var _iP=_iO[1],_iQ=_iO[2];return !A(_iL,[_iM,_iP])?[1,_iP,new T(function(){return _iK(_iL,_iM,_iQ);})]:E(_iQ);}},_iR=function(_iS,_iT,_iU){return _iK(new T(function(){return _36(_iS);}),_iT,_iU);},_iV=function(_iW,_iX,_iY){return (function(_iZ,_j0){while(1){var _j1=E(_j0);if(!_j1[0]){return E(_iZ);}else{var _j2=_iR(_iW,_j1[1],_iZ);_j0=_j1[2];_iZ=_j2;continue;}}})(_iX,_iY);},_j3=[1,_5J],_j4=[0,0],_j5=[0,1],_j6=[0,2],_j7=function(_j8,_j9){while(1){var _ja=E(_j9);if(!_ja[0]){return true;}else{if(!A(_j8,[_ja[1]])){return false;}else{_j9=_ja[2];continue;}}}},_jb=function(_jc,_jd){return _j7(function(_je){var _jf=E(_je),_jg=E(_jf[2])[1];if(_jg>=0){var _jh=E(_jf[1])[1];return _jh>=0?_Z(_Z(new T(function(){return E(E(_jc)[1]);}),_jh),_jg)[0]==0?false:true:E(_it);}else{return E(_it);}},_jd);},_ji=[1,_5I],_jj=function(_jk,_jl){var _jm=E(_jk);if(!_jm){return [0,_t,_jl];}else{var _jn=E(_jl);if(!_jn[0]){return [0,_t,_t];}else{var _jo=new T(function(){var _jp=_jj(_jm-1|0,_jn[2]);return [0,_jp[1],_jp[2]];});return [0,[1,_jn[1],new T(function(){return E(E(_jo)[1]);})],new T(function(){return E(E(_jo)[2]);})];}}},_jq=unCStr("tail"),_jr=new T(function(){return _w(_jq);}),_js=function(_jt,_ju,_jv){if((_ez(_jt,0)-1|0)!=_ju){if(_ju>=0){var _jw=_jj(_ju,_jt);return _e(_jw[1],[1,_jv,new T(function(){var _jx=E(_jw[2]);return _jx[0]==0?E(_jr):E(_jx[2]);})]);}else{return _e(_t,[1,_jv,new T(function(){var _jy=E(_jt);return _jy[0]==0?E(_jr):E(_jy[2]);})]);}}else{return _ju>=0?_e(_jj(_ju,_jt)[1],[1,_jv,_t]):[1,_jv,_t];}},_jz=function(_jA,_jB,_jC){return new T(function(){var _jD=E(_jA)[1],_jE=E(_jB),_jF=E(_jE[1])[1];return _js(_jD,_jF,new T(function(){return _jF>=0?_js(_Z(_jD,_jF),E(_jE[2])[1],_jC):E(_it);}));});},_jG=function(_jH,_jI,_jJ){return [0,_jz(_jH,_jI,_jJ)];},_jK=function(_jL,_jM,_jN){while(1){var _jO=(function(_jP,_jQ,_jR){var _jS=E(_jQ);if(!_jS[0]){return E(_jP);}else{_jL=new T(function(){return _jG(_jP,_jS[1],_jR);});_jM=_jS[2];var _jT=_jR;_jN=_jT;return null;}})(_jL,_jM,_jN);if(_jO!=null){return _jO;}}},_jU=function(_jV,_jW,_jX){var _jY=_iA(_jW,_jX);return _jY[0]==0?[0,new T(function(){return _jK(_jV,[1,_jW,_t],_ji);}),_j4]:!_jb(_jV,_iV(_g2,_f8(_jY[1]),[1,_jW,_t]))?[0,new T(function(){return _jK(_jV,[1,_jW,_t],_j3);}),_j5]:[0,new T(function(){return _jK(_jV,[1,_jW,_t],_j3);}),_j6];},_jZ=function(_k0,_k1){while(1){var _k2=(function(_k3,_k4){var _k5=E(_k4);if(!_k5[0]){return [0];}else{var _k6=_k5[1],_k7=_k5[2];if(!A(_k3,[_k6])){var _k8=_k3;_k1=_k7;_k0=_k8;return null;}else{return [1,_k6,new T(function(){return _jZ(_k3,_k7);})];}}})(_k0,_k1);if(_k2!=null){return _k2;}}},_k9=function(_ka){var _kb=E(_ka);return _kb[0]==0?false:E(_kb[1]);},_kc=function(_kd){var _ke=E(_kd);return _ke[0]==0?[0]:_e(_jZ(_k9,_ke[1]),new T(function(){return _kc(_ke[2]);}));},_kf=unCStr(" shots!"),_kg=function(_kh,_){message(toJSStr(unAppCStr("The computer beat you with ",new T(function(){return _e(_o(0,E(_kh)[1],_t),_kf);}))));return _0;},_ki=new T(function(){return _2l("UI.hs:(173,3)-(176,45)|case");}),_kj=unCStr("Sunk one. Haha!"),_kk=[0,45],_kl=unCStr("Darn. Couldnt kill you!"),_km=function(_kn,_ko,_kp,_kq,_){var _kr=E(_kq);if(!_kr[0]){message(toJSStr(E(_kl)));return _0;}else{var _ks=_kr[1],_kt=_jU(_ko,_ks,_kp),_ku=new T(function(){var _kv=E(_ks);return _e(_o(0,E(_kv[1])[1],_t),[1,_kk,new T(function(){return _o(0,E(_kv[2])[1],_t);})]);}),_kw=function(_){return _ez(_kc(E(_ko)[1]),0)==19?_kg(new T(function(){return [0,E(_kn)[1]+1|0];}),_):_km(new T(function(){return [0,E(_kn)[1]+1|0];}),_kt[1],_kp,_kr[2],_);};switch(E(E(_kt[2])[1])){case 0:markMiss(toJSStr(E(_ku)));return _kw(_);case 1:markHit(toJSStr(E(_ku)));return _kw(_);case 2:debug(toJSStr(E(_kj)));return _kw(_);default:return E(_ki);}}},_kx=function(_ky){var _kz=function(_kA){var _kB=E(_kA);return _kB[0]==0?E(new T(function(){var _kC=E(_ky);return _kC==9?[0]:_kx(_kC+1|0);})):[1,[0,[0,_ky],_kB[1]],new T(function(){return _kz(_kB[2]);})];};return _kz(_fo);},_kD=new T(function(){return _kx(0);}),_kE=new T(function(){var _kF=jsRound(42);return [0,_kF];}),_kG=function(_kH){return [0,(imul(69069,E(_kH)[1])|0)+1|0];},_kI=unCStr("base"),_kJ=unCStr("GHC.Exception"),_kK=unCStr("ArithException"),_kL=[0,4194982440,3110813675,_kI,_kJ,_kK],_kM=[0,4194982440,3110813675,_kL,_t],_kN=function(_kO){return E(_kM);},_kP=function(_kQ){var _kR=E(_kQ);return _1j(_1f(_kR[1]),_kN,_kR[2]);},_kS=unCStr("arithmetic underflow"),_kT=unCStr("arithmetic overflow"),_kU=unCStr("Ratio has zero denominator"),_kV=unCStr("denormal"),_kW=unCStr("divide by zero"),_kX=unCStr("loss of precision"),_kY=function(_kZ){switch(E(_kZ)){case 0:return E(_kT);case 1:return E(_kS);case 2:return E(_kX);case 3:return E(_kW);case 4:return E(_kV);default:return E(_kU);}},_l0=function(_l1){return _e(_kS,_l1);},_l2=function(_l1){return _e(_kT,_l1);},_l3=function(_l1){return _e(_kU,_l1);},_l4=function(_l1){return _e(_kV,_l1);},_l5=function(_l1){return _e(_kW,_l1);},_l6=function(_l1){return _e(_kX,_l1);},_l7=function(_l8){switch(E(_l8)){case 0:return E(_l2);case 1:return E(_l0);case 2:return E(_l6);case 3:return E(_l5);case 4:return E(_l4);default:return E(_l3);}},_l9=function(_la,_lb){return _1D(_l7,_la,_lb);},_lc=function(_ld,_le){switch(E(_le)){case 0:return E(_l2);case 1:return E(_l0);case 2:return E(_l6);case 3:return E(_l5);case 4:return E(_l4);default:return E(_l3);}},_lf=[0,_lc,_kY,_l9],_lg=[0,_kN,_lf,_lh,_kP],_lh=function(_l1){return [0,_lg,_l1];},_li=3,_lj=new T(function(){return _1X(_li,_lh);}),_lk=function(_ll,_lm){var _ln=E(_ll),_lo=E(_lm);return E(_ln[1])[1]!=E(_lo[1])[1]?false:_fc(_ln[2],_lo[2]);},_lp=function(_lq,_lr){var _ls=_lq%_lr;if(_lq<=0){if(_lq>=0){return E(_ls);}else{if(_lr<=0){return E(_ls);}else{var _lt=E(_ls);return _lt==0?0:_lt+_lr|0;}}}else{if(_lr>=0){if(_lq>=0){return E(_ls);}else{if(_lr<=0){return E(_ls);}else{var _lu=E(_ls);return _lu==0?0:_lu+_lr|0;}}}else{var _lv=E(_ls);return _lv==0?0:_lv+_lr|0;}}},_lw=function(_lx,_ly){var _lz=E(_ly);if(!_lz[0]){return [0];}else{var _lA=new T(function(){var _lB=_ez(_lz,0)-1|0;switch(_lB){case -1:return _Z(_lz,0);case 0:return E(_lj);default:var _lC=_lp((imul(214013,E(_lx)[1])|0)+2531011|0,_lB);return _lC>=0?_Z(_lz,_lC):E(_it);}});return [1,_lA,new T(function(){return _lw(new T(function(){return _kG(_lx);}),_iK(_lk,_lA,_lz));})];}},_lD=new T(function(){return _lw(_kE,_kD);}),_lE=[0,0],_lF=unCStr("Fleet not yet valid, game can\'t be started!"),_lG=function(_lH,_){var _lI=rMV(_lH),_lJ=E(E(_lI)[1])[1];if(!_if(_lJ)){message(toJSStr(E(_lF)));return _0;}else{return _km(_lE,_ir,_lJ,_lD,_);}},_lK=function(_lL,_lM,_lN,_){return _lG(E(_lN)[1],_);},_lO=function(_lP,_){var _lQ=js_jquery(toJSStr(E(_hK)));js_click(_lQ,function(_lR,_lS,_){return _gI(_lR,_lS,_lP,_);});var _lT=js_jquery(toJSStr(E(_hL)));js_click(_lT,function(_lU,_lV,_){return _gX(_lU,_lV,_lP,_);});var _lW=js_jquery(toJSStr(E(_hy)));js_click(_lW,function(_lX,_lY,_){return _hu(_lX,_lY,_lP,_);});var _lZ=js_jquery(toJSStr(E(_hz)));js_click(_lZ,function(_m0,_m1,_){return _lK(_m0,_m1,_lP,_);});var _m2=js_jquery(toJSStr(E(_hJ)));js_click(_m2,E(_hI)[1]);var _m3=js_jquery(toJSStr(E(_hE)));js_click(_m3,E(_hD)[1]);return _0;},_m4=[0,_t],_m5=[0,_m4],_m6=function(_){var _m7=nMV(_m5);return _lO([0,_m7],_);},_m8=function(_){return _m6(_);};
var hasteMain = function() {A(_m8, [0]);};window.onload = hasteMain;