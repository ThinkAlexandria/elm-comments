(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}



// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (!x.$)
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800)
			+
			String.fromCharCode(code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

var _Json_decodeInt = { $: 2 };
var _Json_decodeBool = { $: 3 };
var _Json_decodeFloat = { $: 4 };
var _Json_decodeValue = { $: 5 };
var _Json_decodeString = { $: 6 };

function _Json_decodeList(decoder) { return { $: 7, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 8, b: decoder }; }

function _Json_decodeNull(value) { return { $: 9, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 10,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 11,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 12,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 13,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 14,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 15,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 3:
			return (typeof value === 'boolean')
				? elm$core$Result$Ok(value)
				: _Json_expecting('a BOOL', value);

		case 2:
			if (typeof value !== 'number') {
				return _Json_expecting('an INT', value);
			}

			if (-2147483647 < value && value < 2147483647 && (value | 0) === value) {
				return elm$core$Result$Ok(value);
			}

			if (isFinite(value) && !(value % 1)) {
				return elm$core$Result$Ok(value);
			}

			return _Json_expecting('an INT', value);

		case 4:
			return (typeof value === 'number')
				? elm$core$Result$Ok(value)
				: _Json_expecting('a FLOAT', value);

		case 6:
			return (typeof value === 'string')
				? elm$core$Result$Ok(value)
				: (value instanceof String)
					? elm$core$Result$Ok(value + '')
					: _Json_expecting('a STRING', value);

		case 9:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 5:
			return elm$core$Result$Ok(_Json_wrap(value));

		case 7:
			if (!Array.isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 8:
			if (!Array.isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 10:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 11:
			var index = decoder.e;
			if (!Array.isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 12:
			if (typeof value !== 'object' || value === null || Array.isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 13:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 14:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 15:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 3:
		case 2:
		case 4:
		case 6:
		case 5:
			return true;

		case 9:
			return x.c === y.c;

		case 7:
		case 8:
		case 12:
			return _Json_equality(x.b, y.b);

		case 10:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 11:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 13:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 14:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 15:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



// SEND REQUEST

var _Http_toTask = F2(function(request, maybeProgress)
{
	return _Scheduler_binding(function(callback)
	{
		var xhr = new XMLHttpRequest();

		_Http_configureProgress(xhr, maybeProgress);

		xhr.addEventListener('error', function() {
			callback(_Scheduler_fail(elm$http$Http$NetworkError));
		});
		xhr.addEventListener('timeout', function() {
			callback(_Scheduler_fail(elm$http$Http$Timeout));
		});
		xhr.addEventListener('load', function() {
			callback(_Http_handleResponse(xhr, request.expect.a));
		});

		try
		{
			xhr.open(request.method, request.url, true);
		}
		catch (e)
		{
			return callback(_Scheduler_fail(elm$http$Http$BadUrl(request.url)));
		}

		_Http_configureRequest(xhr, request);

		var body = request.body;
		xhr.send(elm$http$Http$Internal$isStringBody(body)
			? (xhr.setRequestHeader('Content-Type', body.a), body.b)
			: body.a
		);

		return function() { xhr.abort(); };
	});
});

function _Http_configureProgress(xhr, maybeProgress)
{
	if (!elm$core$Maybe$isJust(maybeProgress))
	{
		return;
	}

	xhr.addEventListener('progress', function(event) {
		if (!event.lengthComputable)
		{
			return;
		}
		_Scheduler_rawSpawn(maybeProgress.a({
			bytes: event.loaded,
			bytesExpected: event.total
		}));
	});
}

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}

	xhr.responseType = request.expect.b;
	xhr.withCredentials = request.withCredentials;

	elm$core$Maybe$isJust(request.timeout) && (xhr.timeout = request.timeout.a);
}


// RESPONSES

function _Http_handleResponse(xhr, responseToResult)
{
	var response = _Http_toResponse(xhr);

	if (xhr.status < 200 || 300 <= xhr.status)
	{
		response.body = xhr.responseText;
		return _Scheduler_fail(elm$http$Http$BadStatus(response));
	}

	var result = responseToResult(response);

	if (elm$core$Result$isOk(result))
	{
		return _Scheduler_succeed(result.a);
	}
	else
	{
		response.body = xhr.responseText;
		return _Scheduler_fail(A2(elm$http$Http$BadPayload, result.a, response));
	}
}

function _Http_toResponse(xhr)
{
	return {
		url: xhr.responseURL,
		status: { code: xhr.status, message: xhr.statusText },
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders()),
		body: xhr.response
	};
}

function _Http_parseHeaders(rawHeaders)
{
	var headers = elm$core$Dict$empty;

	if (!rawHeaders)
	{
		return headers;
	}

	var headerPairs = rawHeaders.split('\u000d\u000a');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf('\u003a\u0020');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3(elm$core$Dict$update, key, function(oldValue) {
				return elm$core$Maybe$Just(elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}

	return headers;
}


// EXPECTORS

function _Http_expectStringResponse(responseToResult)
{
	return {
		$: 0,
		b: 'text',
		a: responseToResult
	};
}

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		b: expect.b,
		a: function(response) {
			var convertedResponse = expect.a(response);
			return A2(elm$core$Result$map, func, convertedResponse);
		}
	};
});


// BODY

function _Http_multipart(parts)
{


	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}

	return elm$http$Http$Internal$FormDataBody(formData);
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		out.push(A4(elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? elm$core$Maybe$Just(submatch)
				: elm$core$Maybe$Nothing;
		}
		return replacer(A4(elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// VIRTUAL-DOM WIDGETS


var _Markdown_toHtml = F3(function(options, factList, rawMarkdown)
{
	return _VirtualDom_custom(
		factList,
		{
			a: options,
			b: rawMarkdown
		},
		_Markdown_render,
		_Markdown_diff
	);
});



// WIDGET IMPLEMENTATION


function _Markdown_render(model)
{
	return A2(_Markdown_replace, model, _VirtualDom_doc.createElement('div'));
}


function _Markdown_diff(x, y)
{
	return x.b === y.b && x.a === y.a
		? false
		: _Markdown_replace(y);
}


var _Markdown_replace = F2(function(model, div)
{
	div.innerHTML = _Markdown_marked(model.b, _Markdown_formatOptions(model.a));
	return div;
});



// ACTUAL MARKDOWN PARSER


var _Markdown_marked = function() {
	// catch the `marked` object regardless of the outer environment.
	// (ex. a CommonJS module compatible environment.)
	// note that this depends on marked's implementation of environment detection.
	var module = {};
	var exports = module.exports = {};

	/**
	 * marked - a markdown parser
	 * Copyright (c) 2011-2014, Christopher Jeffrey. (MIT Licensed)
	 * https://github.com/chjj/marked
	 * commit cd2f6f5b7091154c5526e79b5f3bfb4d15995a51
	 */
	(function(){var block={newline:/^\n+/,code:/^( {4}[^\n]+\n*)+/,fences:noop,hr:/^( *[-*_]){3,} *(?:\n+|$)/,heading:/^ *(#{1,6}) *([^\n]+?) *#* *(?:\n+|$)/,nptable:noop,lheading:/^([^\n]+)\n *(=|-){2,} *(?:\n+|$)/,blockquote:/^( *>[^\n]+(\n(?!def)[^\n]+)*\n*)+/,list:/^( *)(bull) [\s\S]+?(?:hr|def|\n{2,}(?! )(?!\1bull )\n*|\s*$)/,html:/^ *(?:comment *(?:\n|\s*$)|closed *(?:\n{2,}|\s*$)|closing *(?:\n{2,}|\s*$))/,def:/^ *\[([^\]]+)\]: *<?([^\s>]+)>?(?: +["(]([^\n]+)[")])? *(?:\n+|$)/,table:noop,paragraph:/^((?:[^\n]+\n?(?!hr|heading|lheading|blockquote|tag|def))+)\n*/,text:/^[^\n]+/};block.bullet=/(?:[*+-]|\d+\.)/;block.item=/^( *)(bull) [^\n]*(?:\n(?!\1bull )[^\n]*)*/;block.item=replace(block.item,"gm")(/bull/g,block.bullet)();block.list=replace(block.list)(/bull/g,block.bullet)("hr","\\n+(?=\\1?(?:[-*_] *){3,}(?:\\n+|$))")("def","\\n+(?="+block.def.source+")")();block.blockquote=replace(block.blockquote)("def",block.def)();block._tag="(?!(?:"+"a|em|strong|small|s|cite|q|dfn|abbr|data|time|code"+"|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo"+"|span|br|wbr|ins|del|img)\\b)\\w+(?!:/|[^\\w\\s@]*@)\\b";block.html=replace(block.html)("comment",/<!--[\s\S]*?-->/)("closed",/<(tag)[\s\S]+?<\/\1>/)("closing",/<tag(?:"[^"]*"|'[^']*'|[^'">])*?>/)(/tag/g,block._tag)();block.paragraph=replace(block.paragraph)("hr",block.hr)("heading",block.heading)("lheading",block.lheading)("blockquote",block.blockquote)("tag","<"+block._tag)("def",block.def)();block.normal=merge({},block);block.gfm=merge({},block.normal,{fences:/^ *(`{3,}|~{3,})[ \.]*(\S+)? *\n([\s\S]*?)\s*\1 *(?:\n+|$)/,paragraph:/^/,heading:/^ *(#{1,6}) +([^\n]+?) *#* *(?:\n+|$)/});block.gfm.paragraph=replace(block.paragraph)("(?!","(?!"+block.gfm.fences.source.replace("\\1","\\2")+"|"+block.list.source.replace("\\1","\\3")+"|")();block.tables=merge({},block.gfm,{nptable:/^ *(\S.*\|.*)\n *([-:]+ *\|[-| :]*)\n((?:.*\|.*(?:\n|$))*)\n*/,table:/^ *\|(.+)\n *\|( *[-:]+[-| :]*)\n((?: *\|.*(?:\n|$))*)\n*/});function Lexer(options){this.tokens=[];this.tokens.links={};this.options=options||marked.defaults;this.rules=block.normal;if(this.options.gfm){if(this.options.tables){this.rules=block.tables}else{this.rules=block.gfm}}}Lexer.rules=block;Lexer.lex=function(src,options){var lexer=new Lexer(options);return lexer.lex(src)};Lexer.prototype.lex=function(src){src=src.replace(/\r\n|\r/g,"\n").replace(/\t/g,"    ").replace(/\u00a0/g," ").replace(/\u2424/g,"\n");return this.token(src,true)};Lexer.prototype.token=function(src,top,bq){var src=src.replace(/^ +$/gm,""),next,loose,cap,bull,b,item,space,i,l;while(src){if(cap=this.rules.newline.exec(src)){src=src.substring(cap[0].length);if(cap[0].length>1){this.tokens.push({type:"space"})}}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);cap=cap[0].replace(/^ {4}/gm,"");this.tokens.push({type:"code",text:!this.options.pedantic?cap.replace(/\n+$/,""):cap});continue}if(cap=this.rules.fences.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"code",lang:cap[2],text:cap[3]||""});continue}if(cap=this.rules.heading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[1].length,text:cap[2]});continue}if(top&&(cap=this.rules.nptable.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].split(/ *\| */)}this.tokens.push(item);continue}if(cap=this.rules.lheading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[2]==="="?1:2,text:cap[1]});continue}if(cap=this.rules.hr.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"hr"});continue}if(cap=this.rules.blockquote.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"blockquote_start"});cap=cap[0].replace(/^ *> ?/gm,"");this.token(cap,top,true);this.tokens.push({type:"blockquote_end"});continue}if(cap=this.rules.list.exec(src)){src=src.substring(cap[0].length);bull=cap[2];this.tokens.push({type:"list_start",ordered:bull.length>1});cap=cap[0].match(this.rules.item);next=false;l=cap.length;i=0;for(;i<l;i++){item=cap[i];space=item.length;item=item.replace(/^ *([*+-]|\d+\.) +/,"");if(~item.indexOf("\n ")){space-=item.length;item=!this.options.pedantic?item.replace(new RegExp("^ {1,"+space+"}","gm"),""):item.replace(/^ {1,4}/gm,"")}if(this.options.smartLists&&i!==l-1){b=block.bullet.exec(cap[i+1])[0];if(bull!==b&&!(bull.length>1&&b.length>1)){src=cap.slice(i+1).join("\n")+src;i=l-1}}loose=next||/\n\n(?!\s*$)/.test(item);if(i!==l-1){next=item.charAt(item.length-1)==="\n";if(!loose)loose=next}this.tokens.push({type:loose?"loose_item_start":"list_item_start"});this.token(item,false,bq);this.tokens.push({type:"list_item_end"})}this.tokens.push({type:"list_end"});continue}if(cap=this.rules.html.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:this.options.sanitize?"paragraph":"html",pre:!this.options.sanitizer&&(cap[1]==="pre"||cap[1]==="script"||cap[1]==="style"),text:cap[0]});continue}if(!bq&&top&&(cap=this.rules.def.exec(src))){src=src.substring(cap[0].length);this.tokens.links[cap[1].toLowerCase()]={href:cap[2],title:cap[3]};continue}if(top&&(cap=this.rules.table.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/(?: *\| *)?\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].replace(/^ *\| *| *\| *$/g,"").split(/ *\| */)}this.tokens.push(item);continue}if(top&&(cap=this.rules.paragraph.exec(src))){src=src.substring(cap[0].length);this.tokens.push({type:"paragraph",text:cap[1].charAt(cap[1].length-1)==="\n"?cap[1].slice(0,-1):cap[1]});continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"text",text:cap[0]});continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return this.tokens};var inline={escape:/^\\([\\`*{}\[\]()#+\-.!_>])/,autolink:/^<([^ >]+(@|:\/)[^ >]+)>/,url:noop,tag:/^<!--[\s\S]*?-->|^<\/?\w+(?:"[^"]*"|'[^']*'|[^'">])*?>/,link:/^!?\[(inside)\]\(href\)/,reflink:/^!?\[(inside)\]\s*\[([^\]]*)\]/,nolink:/^!?\[((?:\[[^\]]*\]|[^\[\]])*)\]/,strong:/^_\_([\s\S]+?)_\_(?!_)|^\*\*([\s\S]+?)\*\*(?!\*)/,em:/^\b_((?:[^_]|_\_)+?)_\b|^\*((?:\*\*|[\s\S])+?)\*(?!\*)/,code:/^(`+)\s*([\s\S]*?[^`])\s*\1(?!`)/,br:/^ {2,}\n(?!\s*$)/,del:noop,text:/^[\s\S]+?(?=[\\<!\[_*`]| {2,}\n|$)/};inline._inside=/(?:\[[^\]]*\]|[^\[\]]|\](?=[^\[]*\]))*/;inline._href=/\s*<?([\s\S]*?)>?(?:\s+['"]([\s\S]*?)['"])?\s*/;inline.link=replace(inline.link)("inside",inline._inside)("href",inline._href)();inline.reflink=replace(inline.reflink)("inside",inline._inside)();inline.normal=merge({},inline);inline.pedantic=merge({},inline.normal,{strong:/^_\_(?=\S)([\s\S]*?\S)_\_(?!_)|^\*\*(?=\S)([\s\S]*?\S)\*\*(?!\*)/,em:/^_(?=\S)([\s\S]*?\S)_(?!_)|^\*(?=\S)([\s\S]*?\S)\*(?!\*)/});inline.gfm=merge({},inline.normal,{escape:replace(inline.escape)("])","~|])")(),url:/^(https?:\/\/[^\s<]+[^<.,:;"')\]\s])/,del:/^~~(?=\S)([\s\S]*?\S)~~/,text:replace(inline.text)("]|","~]|")("|","|https?://|")()});inline.breaks=merge({},inline.gfm,{br:replace(inline.br)("{2,}","*")(),text:replace(inline.gfm.text)("{2,}","*")()});function InlineLexer(links,options){this.options=options||marked.defaults;this.links=links;this.rules=inline.normal;this.renderer=this.options.renderer||new Renderer;this.renderer.options=this.options;if(!this.links){throw new Error("Tokens array requires a `links` property.")}if(this.options.gfm){if(this.options.breaks){this.rules=inline.breaks}else{this.rules=inline.gfm}}else if(this.options.pedantic){this.rules=inline.pedantic}}InlineLexer.rules=inline;InlineLexer.output=function(src,links,options){var inline=new InlineLexer(links,options);return inline.output(src)};InlineLexer.prototype.output=function(src){var out="",link,text,href,cap;while(src){if(cap=this.rules.escape.exec(src)){src=src.substring(cap[0].length);out+=cap[1];continue}if(cap=this.rules.autolink.exec(src)){src=src.substring(cap[0].length);if(cap[2]==="@"){text=cap[1].charAt(6)===":"?this.mangle(cap[1].substring(7)):this.mangle(cap[1]);href=this.mangle("mailto:")+text}else{text=escape(cap[1]);href=text}out+=this.renderer.link(href,null,text);continue}if(!this.inLink&&(cap=this.rules.url.exec(src))){src=src.substring(cap[0].length);text=escape(cap[1]);href=text;out+=this.renderer.link(href,null,text);continue}if(cap=this.rules.tag.exec(src)){if(!this.inLink&&/^<a /i.test(cap[0])){this.inLink=true}else if(this.inLink&&/^<\/a>/i.test(cap[0])){this.inLink=false}src=src.substring(cap[0].length);out+=this.options.sanitize?this.options.sanitizer?this.options.sanitizer(cap[0]):escape(cap[0]):cap[0];continue}if(cap=this.rules.link.exec(src)){src=src.substring(cap[0].length);this.inLink=true;out+=this.outputLink(cap,{href:cap[2],title:cap[3]});this.inLink=false;continue}if((cap=this.rules.reflink.exec(src))||(cap=this.rules.nolink.exec(src))){src=src.substring(cap[0].length);link=(cap[2]||cap[1]).replace(/\s+/g," ");link=this.links[link.toLowerCase()];if(!link||!link.href){out+=cap[0].charAt(0);src=cap[0].substring(1)+src;continue}this.inLink=true;out+=this.outputLink(cap,link);this.inLink=false;continue}if(cap=this.rules.strong.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.strong(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.em.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.em(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.codespan(escape(cap[2],true));continue}if(cap=this.rules.br.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.br();continue}if(cap=this.rules.del.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.del(this.output(cap[1]));continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.text(escape(this.smartypants(cap[0])));continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return out};InlineLexer.prototype.outputLink=function(cap,link){var href=escape(link.href),title=link.title?escape(link.title):null;return cap[0].charAt(0)!=="!"?this.renderer.link(href,title,this.output(cap[1])):this.renderer.image(href,title,escape(cap[1]))};InlineLexer.prototype.smartypants=function(text){if(!this.options.smartypants)return text;return text.replace(/---/g,"—").replace(/--/g,"–").replace(/(^|[-\u2014\/(\[{"\s])'/g,"$1‘").replace(/'/g,"’").replace(/(^|[-\u2014\/(\[{\u2018\s])"/g,"$1“").replace(/"/g,"”").replace(/\.{3}/g,"…")};InlineLexer.prototype.mangle=function(text){if(!this.options.mangle)return text;var out="",l=text.length,i=0,ch;for(;i<l;i++){ch=text.charCodeAt(i);if(Math.random()>.5){ch="x"+ch.toString(16)}out+="&#"+ch+";"}return out};function Renderer(options){this.options=options||{}}Renderer.prototype.code=function(code,lang,escaped){if(this.options.highlight){var out=this.options.highlight(code,lang);if(out!=null&&out!==code){escaped=true;code=out}}if(!lang){return"<pre><code>"+(escaped?code:escape(code,true))+"\n</code></pre>"}return'<pre><code class="'+this.options.langPrefix+escape(lang,true)+'">'+(escaped?code:escape(code,true))+"\n</code></pre>\n"};Renderer.prototype.blockquote=function(quote){return"<blockquote>\n"+quote+"</blockquote>\n"};Renderer.prototype.html=function(html){return html};Renderer.prototype.heading=function(text,level,raw){return"<h"+level+' id="'+this.options.headerPrefix+raw.toLowerCase().replace(/[^\w]+/g,"-")+'">'+text+"</h"+level+">\n"};Renderer.prototype.hr=function(){return this.options.xhtml?"<hr/>\n":"<hr>\n"};Renderer.prototype.list=function(body,ordered){var type=ordered?"ol":"ul";return"<"+type+">\n"+body+"</"+type+">\n"};Renderer.prototype.listitem=function(text){return"<li>"+text+"</li>\n"};Renderer.prototype.paragraph=function(text){return"<p>"+text+"</p>\n"};Renderer.prototype.table=function(header,body){return"<table>\n"+"<thead>\n"+header+"</thead>\n"+"<tbody>\n"+body+"</tbody>\n"+"</table>\n"};Renderer.prototype.tablerow=function(content){return"<tr>\n"+content+"</tr>\n"};Renderer.prototype.tablecell=function(content,flags){var type=flags.header?"th":"td";var tag=flags.align?"<"+type+' style="text-align:'+flags.align+'">':"<"+type+">";return tag+content+"</"+type+">\n"};Renderer.prototype.strong=function(text){return"<strong>"+text+"</strong>"};Renderer.prototype.em=function(text){return"<em>"+text+"</em>"};Renderer.prototype.codespan=function(text){return"<code>"+text+"</code>"};Renderer.prototype.br=function(){return this.options.xhtml?"<br/>":"<br>"};Renderer.prototype.del=function(text){return"<del>"+text+"</del>"};Renderer.prototype.link=function(href,title,text){if(this.options.sanitize){try{var prot=decodeURIComponent(unescape(href)).replace(/[^\w:]/g,"").toLowerCase()}catch(e){return""}if(prot.indexOf("javascript:")===0||prot.indexOf("vbscript:")===0||prot.indexOf("data:")===0){return""}}var out='<a href="'+href+'"';if(title){out+=' title="'+title+'"'}out+=">"+text+"</a>";return out};Renderer.prototype.image=function(href,title,text){var out='<img src="'+href+'" alt="'+text+'"';if(title){out+=' title="'+title+'"'}out+=this.options.xhtml?"/>":">";return out};Renderer.prototype.text=function(text){return text};function Parser(options){this.tokens=[];this.token=null;this.options=options||marked.defaults;this.options.renderer=this.options.renderer||new Renderer;this.renderer=this.options.renderer;this.renderer.options=this.options}Parser.parse=function(src,options,renderer){var parser=new Parser(options,renderer);return parser.parse(src)};Parser.prototype.parse=function(src){this.inline=new InlineLexer(src.links,this.options,this.renderer);this.tokens=src.reverse();var out="";while(this.next()){out+=this.tok()}return out};Parser.prototype.next=function(){return this.token=this.tokens.pop()};Parser.prototype.peek=function(){return this.tokens[this.tokens.length-1]||0};Parser.prototype.parseText=function(){var body=this.token.text;while(this.peek().type==="text"){body+="\n"+this.next().text}return this.inline.output(body)};Parser.prototype.tok=function(){switch(this.token.type){case"space":{return""}case"hr":{return this.renderer.hr()}case"heading":{return this.renderer.heading(this.inline.output(this.token.text),this.token.depth,this.token.text)}case"code":{return this.renderer.code(this.token.text,this.token.lang,this.token.escaped)}case"table":{var header="",body="",i,row,cell,flags,j;cell="";for(i=0;i<this.token.header.length;i++){flags={header:true,align:this.token.align[i]};cell+=this.renderer.tablecell(this.inline.output(this.token.header[i]),{header:true,align:this.token.align[i]})}header+=this.renderer.tablerow(cell);for(i=0;i<this.token.cells.length;i++){row=this.token.cells[i];cell="";for(j=0;j<row.length;j++){cell+=this.renderer.tablecell(this.inline.output(row[j]),{header:false,align:this.token.align[j]})}body+=this.renderer.tablerow(cell)}return this.renderer.table(header,body)}case"blockquote_start":{var body="";while(this.next().type!=="blockquote_end"){body+=this.tok()}return this.renderer.blockquote(body)}case"list_start":{var body="",ordered=this.token.ordered;while(this.next().type!=="list_end"){body+=this.tok()}return this.renderer.list(body,ordered)}case"list_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.token.type==="text"?this.parseText():this.tok()}return this.renderer.listitem(body)}case"loose_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.tok()}return this.renderer.listitem(body)}case"html":{var html=!this.token.pre&&!this.options.pedantic?this.inline.output(this.token.text):this.token.text;return this.renderer.html(html)}case"paragraph":{return this.renderer.paragraph(this.inline.output(this.token.text))}case"text":{return this.renderer.paragraph(this.parseText())}}};function escape(html,encode){return html.replace(!encode?/&(?!#?\w+;)/g:/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#39;")}function unescape(html){return html.replace(/&(#(?:\d+)|(?:#x[0-9A-Fa-f]+)|(?:\w+));?/g,function(_,n){n=n.toLowerCase();if(n==="colon")return":";if(n.charAt(0)==="#"){return n.charAt(1)==="x"?String.fromCharCode(parseInt(n.substring(2),16)):String.fromCharCode(+n.substring(1))}return""})}function replace(regex,opt){regex=regex.source;opt=opt||"";return function self(name,val){if(!name)return new RegExp(regex,opt);val=val.source||val;val=val.replace(/(^|[^\[])\^/g,"$1");regex=regex.replace(name,val);return self}}function noop(){}noop.exec=noop;function merge(obj){var i=1,target,key;for(;i<arguments.length;i++){target=arguments[i];for(key in target){if(Object.prototype.hasOwnProperty.call(target,key)){obj[key]=target[key]}}}return obj}function marked(src,opt,callback){if(callback||typeof opt==="function"){if(!callback){callback=opt;opt=null}opt=merge({},marked.defaults,opt||{});var highlight=opt.highlight,tokens,pending,i=0;try{tokens=Lexer.lex(src,opt)}catch(e){return callback(e)}pending=tokens.length;var done=function(err){if(err){opt.highlight=highlight;return callback(err)}var out;try{out=Parser.parse(tokens,opt)}catch(e){err=e}opt.highlight=highlight;return err?callback(err):callback(null,out)};if(!highlight||highlight.length<3){return done()}delete opt.highlight;if(!pending)return done();for(;i<tokens.length;i++){(function(token){if(token.type!=="code"){return--pending||done()}return highlight(token.text,token.lang,function(err,code){if(err)return done(err);if(code==null||code===token.text){return--pending||done()}token.text=code;token.escaped=true;--pending||done()})})(tokens[i])}return}try{if(opt)opt=merge({},marked.defaults,opt);return Parser.parse(Lexer.lex(src,opt),opt)}catch(e){e.message+="\nPlease report this to https://github.com/chjj/marked.";if((opt||marked.defaults).silent){return"<p>An error occured:</p><pre>"+escape(e.message+"",true)+"</pre>"}throw e}}marked.options=marked.setOptions=function(opt){merge(marked.defaults,opt);return marked};marked.defaults={gfm:true,tables:true,breaks:false,pedantic:false,sanitize:false,sanitizer:null,mangle:true,smartLists:false,silent:false,highlight:null,langPrefix:"lang-",smartypants:false,headerPrefix:"",renderer:new Renderer,xhtml:false};marked.Parser=Parser;marked.parser=Parser.parse;marked.Renderer=Renderer;marked.Lexer=Lexer;marked.lexer=Lexer.lex;marked.InlineLexer=InlineLexer;marked.inlineLexer=InlineLexer.output;marked.parse=marked;if(typeof module!=="undefined"&&typeof exports==="object"){module.exports=marked}else if(typeof define==="function"&&define.amd){define(function(){return marked})}else{this.marked=marked}}).call(function(){return this||(typeof window!=="undefined"?window:global)}());

	return module.exports;
}();


// FORMAT OPTIONS FOR MARKED IMPLEMENTATION

function _Markdown_formatOptions(options)
{
	function toHighlight(code, lang)
	{
		if (!lang && elm$core$Maybe$isJust(options.defaultHighlighting))
		{
			lang = options.defaultHighlighting.a;
		}

		if (typeof hljs !== 'undefined' && lang && hljs.listLanguages().indexOf(lang) >= 0)
		{
			return hljs.highlight(lang, code, true).value;
		}

		return code;
	}

	var gfm = options.githubFlavored.a;

	return {
		highlight: toHighlight,
		gfm: gfm,
		tables: gfm && gfm.tables,
		breaks: gfm && gfm.breaks,
		sanitize: options.sanitize,
		smartypants: options.smartypants
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.download)
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}
var ChristophP$elm_i18next$I18Next$Translations = function (a) {
	return {$: 'Translations', a: a};
};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$List$cons = _List_cons;
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$core$Basics$append = _Utils_append;
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Basics$eq = _Utils_equal;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var ChristophP$elm_i18next$I18Next$foldTree = F3(
	function (initialValue, dict, namespace) {
		return A3(
			elm$core$Dict$foldl,
			F3(
				function (key, val, acc) {
					var newNamespace = function (currentKey) {
						return elm$core$String$isEmpty(namespace) ? currentKey : (namespace + ('.' + currentKey));
					};
					if (val.$ === 'Leaf') {
						var str = val.a;
						return A3(
							elm$core$Dict$insert,
							newNamespace(key),
							str,
							acc);
					} else {
						var children = val.a;
						return A3(
							ChristophP$elm_i18next$I18Next$foldTree,
							acc,
							children,
							newNamespace(key));
					}
				}),
			initialValue,
			dict);
	});
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var ChristophP$elm_i18next$I18Next$initialTranslations = ChristophP$elm_i18next$I18Next$Translations(elm$core$Dict$empty);
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var ChristophP$elm_i18next$I18Next$mapTreeToDict = function (tree) {
	if (tree.$ === 'Branch') {
		var dict = tree.a;
		return ChristophP$elm_i18next$I18Next$Translations(
			A3(ChristophP$elm_i18next$I18Next$foldTree, elm$core$Dict$empty, dict, ''));
	} else {
		return ChristophP$elm_i18next$I18Next$initialTranslations;
	}
};
var ChristophP$elm_i18next$I18Next$Branch = function (a) {
	return {$: 'Branch', a: a};
};
var ChristophP$elm_i18next$I18Next$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Basics$toFloat = _Basics_toFloat;
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$add = _Basics_add;
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$idiv = _Basics_idiv;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$or = _Basics_or;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		elm$json$Json$Decode$map,
		elm$core$Dict$fromList,
		elm$json$Json$Decode$keyValuePairs(decoder));
};
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$json$Json$Decode$lazy = function (thunk) {
	return A2(
		elm$json$Json$Decode$andThen,
		thunk,
		elm$json$Json$Decode$succeed(_Utils_Tuple0));
};
var elm$json$Json$Decode$oneOf = _Json_oneOf;
var elm$json$Json$Decode$string = _Json_decodeString;
function ChristophP$elm_i18next$I18Next$cyclic$treeDecoder() {
	return elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2(elm$json$Json$Decode$map, ChristophP$elm_i18next$I18Next$Leaf, elm$json$Json$Decode$string),
				elm$json$Json$Decode$lazy(
				function (_n0) {
					return A2(
						elm$json$Json$Decode$map,
						ChristophP$elm_i18next$I18Next$Branch,
						elm$json$Json$Decode$dict(
							ChristophP$elm_i18next$I18Next$cyclic$treeDecoder()));
				})
			]));
}
try {
	var ChristophP$elm_i18next$I18Next$treeDecoder = ChristophP$elm_i18next$I18Next$cyclic$treeDecoder();
	ChristophP$elm_i18next$I18Next$cyclic$treeDecoder = function () {
		return ChristophP$elm_i18next$I18Next$treeDecoder;
	};
} catch ($) {
throw 'Some top-level definitions from `I18Next` are causing infinite recursion:\n\n  ┌─────┐\n  │    treeDecoder\n  └─────┘\n\nThese errors are very tricky, so read https://elm-lang.org/0.19.0/halting-problem to learn how to fix it!';}
var ChristophP$elm_i18next$I18Next$translationsDecoder = A2(elm$json$Json$Decode$map, ChristophP$elm_i18next$I18Next$mapTreeToDict, ChristophP$elm_i18next$I18Next$treeDecoder);
var elm$http$Http$Internal$EmptyBody = {$: 'EmptyBody'};
var elm$http$Http$emptyBody = elm$http$Http$Internal$EmptyBody;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var lLeft = _n1.d;
			var lRight = _n1.e;
			var _n2 = dict.e;
			var rClr = _n2.a;
			var rK = _n2.b;
			var rV = _n2.c;
			var rLeft = _n2.d;
			var _n3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _n2.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n4 = dict.d;
			var lClr = _n4.a;
			var lK = _n4.b;
			var lV = _n4.c;
			var lLeft = _n4.d;
			var lRight = _n4.e;
			var _n5 = dict.e;
			var rClr = _n5.a;
			var rK = _n5.b;
			var rV = _n5.c;
			var rLeft = _n5.d;
			var rRight = _n5.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n1 = dict.d;
			var lClr = _n1.a;
			var lK = _n1.b;
			var lV = _n1.c;
			var _n2 = _n1.d;
			var _n3 = _n2.a;
			var llK = _n2.b;
			var llV = _n2.c;
			var llLeft = _n2.d;
			var llRight = _n2.e;
			var lRight = _n1.e;
			var _n4 = dict.e;
			var rClr = _n4.a;
			var rK = _n4.b;
			var rV = _n4.c;
			var rLeft = _n4.d;
			var rRight = _n4.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				elm$core$Dict$Red,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _n5 = dict.d;
			var lClr = _n5.a;
			var lK = _n5.b;
			var lV = _n5.c;
			var lLeft = _n5.d;
			var lRight = _n5.e;
			var _n6 = dict.e;
			var rClr = _n6.a;
			var rK = _n6.b;
			var rV = _n6.c;
			var rLeft = _n6.d;
			var rRight = _n6.e;
			if (clr.$ === 'Black') {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Black,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _n1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _n3 = right.a;
							var _n4 = right.d;
							var _n5 = _n4.a;
							return elm$core$Dict$moveRedRight(dict);
						} else {
							break _n2$2;
						}
					} else {
						var _n6 = right.a;
						var _n7 = right.d;
						return elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _n2$2;
				}
			}
			return dict;
		}
	});
var elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _n3 = lLeft.a;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					elm$core$Dict$removeMin(left),
					right);
			} else {
				var _n4 = elm$core$Dict$moveRedLeft(dict);
				if (_n4.$ === 'RBNode_elm_builtin') {
					var nColor = _n4.a;
					var nKey = _n4.b;
					var nValue = _n4.c;
					var nLeft = _n4.d;
					var nRight = _n4.e;
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _n6 = lLeft.a;
						return A5(
							elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2(elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _n7 = elm$core$Dict$moveRedLeft(dict);
						if (_n7.$ === 'RBNode_elm_builtin') {
							var nColor = _n7.a;
							var nKey = _n7.b;
							var nValue = _n7.c;
							var nLeft = _n7.d;
							var nRight = _n7.e;
							return A5(
								elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2(elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2(elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7(elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === 'RBNode_elm_builtin') {
					var minKey = _n1.b;
					var minValue = _n1.c;
					return A5(
						elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						elm$core$Dict$removeMin(right));
				} else {
					return elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2(elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var elm$core$Dict$remove = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$removeHelp, key, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (_n0.$ === 'Just') {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadPayload = F2(
	function (a, b) {
		return {$: 'BadPayload', a: a, b: b};
	});
var elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var elm$http$Http$NetworkError = {$: 'NetworkError'};
var elm$http$Http$Timeout = {$: 'Timeout'};
var elm$http$Http$Internal$FormDataBody = function (a) {
	return {$: 'FormDataBody', a: a};
};
var elm$http$Http$Internal$isStringBody = function (body) {
	if (body.$ === 'StringBody') {
		return true;
	} else {
		return false;
	}
};
var elm$http$Http$expectStringResponse = _Http_expectStringResponse;
var elm$json$Json$Decode$decodeString = _Json_runOnString;
var elm$http$Http$expectJson = function (decoder) {
	return elm$http$Http$expectStringResponse(
		function (response) {
			var _n0 = A2(elm$json$Json$Decode$decodeString, decoder, response.body);
			if (_n0.$ === 'Err') {
				var decodeError = _n0.a;
				return elm$core$Result$Err(
					elm$json$Json$Decode$errorToString(decodeError));
			} else {
				var value = _n0.a;
				return elm$core$Result$Ok(value);
			}
		});
};
var elm$http$Http$Internal$Request = function (a) {
	return {$: 'Request', a: a};
};
var elm$http$Http$request = elm$http$Http$Internal$Request;
var elm$http$Http$get = F2(
	function (url, decoder) {
		return elm$http$Http$request(
			{
				body: elm$http$Http$emptyBody,
				expect: elm$http$Http$expectJson(decoder),
				headers: _List_Nil,
				method: 'GET',
				timeout: elm$core$Maybe$Nothing,
				url: url,
				withCredentials: false
			});
	});
var ChristophP$elm_i18next$I18Next$translationRequest = function (url) {
	return A2(elm$http$Http$get, url, ChristophP$elm_i18next$I18Next$translationsDecoder);
};
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$onError = _Scheduler_onError;
var elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(
					elm$core$Task$onError,
					A2(
						elm$core$Basics$composeL,
						A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
						elm$core$Result$Err),
					A2(
						elm$core$Task$andThen,
						A2(
							elm$core$Basics$composeL,
							A2(elm$core$Basics$composeL, elm$core$Task$succeed, resultToMessage),
							elm$core$Result$Ok),
						task))));
	});
var elm$http$Http$toTask = function (_n0) {
	var request_ = _n0.a;
	return A2(_Http_toTask, request_, elm$core$Maybe$Nothing);
};
var elm$http$Http$send = F2(
	function (resultToMessage, request_) {
		return A2(
			elm$core$Task$attempt,
			resultToMessage,
			elm$http$Http$toTask(request_));
	});
var ChristophP$elm_i18next$I18Next$fetchTranslations = F2(
	function (msg, url) {
		return A2(
			elm$http$Http$send,
			msg,
			ChristophP$elm_i18next$I18Next$translationRequest(url));
	});
var author$project$Comments$defaultState = {editCommentDrafts: elm$core$Dict$empty, newCommentDrafts: elm$core$Dict$empty};
var author$project$Example$TranslationsLoaded = function (a) {
	return {$: 'TranslationsLoaded', a: a};
};
var elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, list);
			var jsArray = _n0.a;
			var remainingItems = _n0.b;
			if (_Utils_cmp(
				elm$core$Elm$JsArray$length(jsArray),
				elm$core$Array$branchFactor) < 0) {
				return A2(
					elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					elm$core$List$cons,
					elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return elm$core$Array$empty;
	} else {
		return A3(elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var elm$time$Time$millisToPosix = elm$time$Time$Posix;
var author$project$Example$examplePost = {
	comments: elm$core$Array$fromList(
		_List_fromArray(
			[
				{
				markdown: '# This is an example comment',
				metadata: {
					createdBy: 'Steve',
					createdTimestamp: elm$time$Time$millisToPosix(1500000000),
					isDeleted: false,
					modifiedBy: 'Steve',
					modifiedTimestamp: elm$time$Time$millisToPosix(1500000000)
				}
			}
			])),
	text: 'You can edit multiple comments at once. Try it!',
	title: 'Example Post'
};
var author$project$Example$init = function (_n0) {
	return _Utils_Tuple2(
		{
			commentState: author$project$Comments$defaultState,
			feed: elm$core$Array$fromList(
				_List_fromArray(
					[author$project$Example$examplePost])),
			translations: ChristophP$elm_i18next$I18Next$initialTranslations
		},
		A2(ChristophP$elm_i18next$I18Next$fetchTranslations, author$project$Example$TranslationsLoaded, 'en.json'));
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Example$subscriptions = function (model) {
	return elm$core$Platform$Sub$none;
};
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var author$project$Comments$getEditDraft = F3(
	function (key, commentIndex, state) {
		return A2(
			elm$core$Maybe$andThen,
			function (x) {
				return elm$core$Maybe$Just(x.markdown);
			},
			A2(
				elm$core$Maybe$andThen,
				elm$core$Dict$get(commentIndex),
				A2(elm$core$Dict$get, key, state.editCommentDrafts)));
	});
var author$project$Comments$getNewDraft = F2(
	function (key, state) {
		return A2(
			elm$core$Maybe$andThen,
			function (x) {
				return elm$core$Maybe$Just(x.markdown);
			},
			A2(elm$core$Dict$get, key, state.newCommentDrafts));
	});
var elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var author$project$Comments$removeEditDraft = F3(
	function (key, commentIndex, state) {
		var oldRow = A2(
			elm$core$Maybe$withDefault,
			elm$core$Dict$empty,
			A2(elm$core$Dict$get, key, state.editCommentDrafts));
		var newRow = A2(elm$core$Dict$remove, commentIndex, oldRow);
		return _Utils_update(
			state,
			{
				editCommentDrafts: elm$core$Dict$isEmpty(newRow) ? A2(elm$core$Dict$remove, key, state.editCommentDrafts) : A3(elm$core$Dict$insert, key, newRow, state.editCommentDrafts)
			});
	});
var author$project$Comments$removeNewDraft = F2(
	function (key, state) {
		return _Utils_update(
			state,
			{
				newCommentDrafts: A2(elm$core$Dict$remove, key, state.newCommentDrafts)
			});
	});
var author$project$Comments$defaultCommentDraft = {markdown: '', showMarkdownPreview: false};
var author$project$Comments$insertEditDraft = F4(
	function (key, commentIndex, commentText, state) {
		var oldRow = A2(
			elm$core$Maybe$withDefault,
			elm$core$Dict$empty,
			A2(elm$core$Dict$get, key, state.editCommentDrafts));
		var draft = {markdown: commentText, showMarkdownPreview: false};
		return _Utils_update(
			state,
			{
				editCommentDrafts: A3(
					elm$core$Dict$insert,
					key,
					A3(elm$core$Dict$insert, commentIndex, draft, oldRow),
					state.editCommentDrafts)
			});
	});
var author$project$Comments$setDraftMarkdown = F2(
	function (x, draft) {
		return _Utils_update(
			draft,
			{markdown: x});
	});
var author$project$Comments$setDraftPreviewFlag = F2(
	function (flag, draft) {
		return _Utils_update(
			draft,
			{showMarkdownPreview: flag});
	});
var elm$core$Dict$member = F2(
	function (key, dict) {
		var _n0 = A2(elm$core$Dict$get, key, dict);
		if (_n0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Comments$update = F3(
	function (createDraftFromExisting, msg, state) {
		switch (msg.$) {
			case 'DeleteCommentDraft':
				var selector = msg.a;
				if (selector.$ === 'Existing') {
					var key = selector.a;
					var commentIndex = selector.b;
					return _Utils_Tuple2(
						A3(author$project$Comments$removeEditDraft, key, commentIndex, state),
						elm$core$Platform$Cmd$none);
				} else {
					var key = selector.a;
					return _Utils_Tuple2(
						_Utils_update(
							state,
							{
								newCommentDrafts: A2(elm$core$Dict$remove, key, state.newCommentDrafts)
							}),
						elm$core$Platform$Cmd$none);
				}
			case 'CreateCommentDraft':
				var selector = msg.a;
				if (selector.$ === 'Existing') {
					var key = selector.a;
					var commentIndex = selector.b;
					var _n3 = A3(author$project$Comments$getEditDraft, key, commentIndex, state);
					if (_n3.$ === 'Just') {
						return _Utils_Tuple2(state, elm$core$Platform$Cmd$none);
					} else {
						var _n4 = A2(createDraftFromExisting, key, commentIndex);
						if (_n4.$ === 'Just') {
							var commentToEdit = _n4.a;
							return _Utils_Tuple2(
								A4(author$project$Comments$insertEditDraft, key, commentIndex, commentToEdit, state),
								elm$core$Platform$Cmd$none);
						} else {
							return _Utils_Tuple2(state, elm$core$Platform$Cmd$none);
						}
					}
				} else {
					var key = selector.a;
					return A2(elm$core$Dict$member, key, state.newCommentDrafts) ? _Utils_Tuple2(state, elm$core$Platform$Cmd$none) : _Utils_Tuple2(
						_Utils_update(
							state,
							{
								newCommentDrafts: A3(elm$core$Dict$insert, key, author$project$Comments$defaultCommentDraft, state.newCommentDrafts)
							}),
						elm$core$Platform$Cmd$none);
				}
			case 'UpdateCommentDraft':
				var selector = msg.a;
				var value = msg.b;
				if (selector.$ === 'Existing') {
					var key = selector.a;
					var commentIndex = selector.b;
					return _Utils_Tuple2(
						_Utils_update(
							state,
							{
								editCommentDrafts: A3(
									elm$core$Dict$update,
									key,
									elm$core$Maybe$map(
										A2(
											elm$core$Dict$update,
											commentIndex,
											elm$core$Maybe$map(
												author$project$Comments$setDraftMarkdown(value)))),
									state.editCommentDrafts)
							}),
						elm$core$Platform$Cmd$none);
				} else {
					var key = selector.a;
					return _Utils_Tuple2(
						_Utils_update(
							state,
							{
								newCommentDrafts: A3(
									elm$core$Dict$update,
									key,
									elm$core$Maybe$map(
										author$project$Comments$setDraftMarkdown(value)),
									state.newCommentDrafts)
							}),
						elm$core$Platform$Cmd$none);
				}
			default:
				var selector = msg.a;
				var showMarkdownPreview = msg.b;
				if (selector.$ === 'Existing') {
					var key = selector.a;
					var commentIndex = selector.b;
					return _Utils_Tuple2(
						_Utils_update(
							state,
							{
								editCommentDrafts: A3(
									elm$core$Dict$update,
									key,
									elm$core$Maybe$map(
										A2(
											elm$core$Dict$update,
											commentIndex,
											elm$core$Maybe$map(
												author$project$Comments$setDraftPreviewFlag(showMarkdownPreview)))),
									state.editCommentDrafts)
							}),
						elm$core$Platform$Cmd$none);
				} else {
					var key = selector.a;
					return _Utils_Tuple2(
						_Utils_update(
							state,
							{
								newCommentDrafts: A3(
									elm$core$Dict$update,
									key,
									elm$core$Maybe$map(
										author$project$Comments$setDraftPreviewFlag(showMarkdownPreview)),
									state.newCommentDrafts)
							}),
						elm$core$Platform$Cmd$none);
				}
		}
	});
var author$project$Example$CommentsMsg = function (a) {
	return {$: 'CommentsMsg', a: a};
};
var elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var elm$core$Array$bitMask = 4294967295 >>> (32 - elm$core$Array$shiftStep);
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = elm$core$Array$bitMask & (index >>> shift);
			var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_n0.$ === 'SubTree') {
				var subTree = _n0.a;
				var $temp$shift = shift - elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _n0.a;
				return A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, values);
			}
		}
	});
var elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Array$get = F2(
	function (index, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? elm$core$Maybe$Just(
			A2(elm$core$Elm$JsArray$unsafeGet, elm$core$Array$bitMask & index, tail)) : elm$core$Maybe$Just(
			A3(elm$core$Array$getHelp, startShift, index, tree)));
	});
var author$project$Example$draftFromExistingComment = F3(
	function (feed, postIndex, commentIndex) {
		return A2(
			elm$core$Maybe$andThen,
			function (y) {
				return elm$core$Maybe$Just(y.markdown);
			},
			A2(
				elm$core$Maybe$andThen,
				function (x) {
					return A2(elm$core$Array$get, commentIndex, x.comments);
				},
				A2(elm$core$Array$get, postIndex, feed)));
	});
var elm$core$Elm$JsArray$push = _JsArray_push;
var elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					elm$core$Elm$JsArray$push,
					elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = elm$core$Array$SubTree(
					A4(elm$core$Array$insertTailInTree, shift - elm$core$Array$shiftStep, index, tail, elm$core$Elm$JsArray$empty));
				return A2(elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = elm$core$Array$SubTree(
					A4(elm$core$Array$insertTailInTree, shift - elm$core$Array$shiftStep, index, tail, subTree));
				return A3(elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = elm$core$Array$SubTree(
					A4(
						elm$core$Array$insertTailInTree,
						shift - elm$core$Array$shiftStep,
						index,
						tail,
						elm$core$Elm$JsArray$singleton(value)));
				return A3(elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _n0) {
		var len = _n0.a;
		var startShift = _n0.b;
		var tree = _n0.c;
		var tail = _n0.d;
		var originalTailLen = elm$core$Elm$JsArray$length(tail);
		var newTailLen = elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + elm$core$Array$shiftStep;
				var newTree = A4(
					elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					elm$core$Elm$JsArray$singleton(
						elm$core$Array$SubTree(tree)));
				return A4(elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4(elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4(elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			elm$core$Array$unsafeReplaceTail,
			A2(elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = elm$core$Array$bitMask & (index >>> shift);
		var _n0 = A2(elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_n0.$ === 'SubTree') {
			var subTree = _n0.a;
			var newSub = A4(elm$core$Array$setHelp, shift - elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _n0.a;
			var newLeaf = A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, values);
			return A3(
				elm$core$Elm$JsArray$unsafeSet,
				pos,
				elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			elm$core$Array$tailIndex(len)) > -1) ? A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3(elm$core$Elm$JsArray$unsafeSet, elm$core$Array$bitMask & index, value, tail)) : A4(
			elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4(elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var elm$core$Platform$Cmd$map = _Platform_map;
var author$project$Example$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'TranslationsLoaded':
				if (msg.a.$ === 'Ok') {
					var translations = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{translations: translations}),
						elm$core$Platform$Cmd$none);
				} else {
					var messag = msg.a.a;
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				}
			case 'CommentsMsg':
				var commentsMsg = msg.a;
				switch (commentsMsg.$) {
					case 'Internal':
						var internalMsg = commentsMsg.a;
						var _n2 = A3(
							author$project$Comments$update,
							author$project$Example$draftFromExistingComment(model.feed),
							internalMsg,
							model.commentState);
						var newCommentState = _n2.a;
						var commentCmd = _n2.b;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{commentState: newCommentState}),
							A2(elm$core$Platform$Cmd$map, author$project$Example$CommentsMsg, commentCmd));
					case 'NewComment':
						var postIndex = commentsMsg.a;
						var _n3 = A2(elm$core$Array$get, postIndex, model.feed);
						if (_n3.$ === 'Just') {
							var post = _n3.a;
							var newComment = A2(author$project$Comments$getNewDraft, postIndex, model.commentState);
							var newPost = _Utils_update(
								post,
								{
									comments: A2(
										elm$core$Array$push,
										{
											markdown: A2(elm$core$Maybe$withDefault, '', newComment),
											metadata: {
												createdBy: 'Steve',
												createdTimestamp: elm$time$Time$millisToPosix(1500000000),
												isDeleted: false,
												modifiedBy: 'Steve',
												modifiedTimestamp: elm$time$Time$millisToPosix(1500000000)
											}
										},
										post.comments)
								});
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										commentState: A2(author$project$Comments$removeNewDraft, postIndex, model.commentState),
										feed: A3(elm$core$Array$set, postIndex, newPost, model.feed)
									}),
								elm$core$Platform$Cmd$none);
						} else {
							return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
						}
					case 'UpdateComment':
						var postIndex = commentsMsg.a;
						var commentIndex = commentsMsg.b;
						var _n4 = A2(elm$core$Array$get, postIndex, model.feed);
						if (_n4.$ === 'Just') {
							var post = _n4.a;
							var _n5 = A2(elm$core$Array$get, commentIndex, post.comments);
							if (_n5.$ === 'Just') {
								var comment = _n5.a;
								var updateComment = A3(author$project$Comments$getEditDraft, postIndex, commentIndex, model.commentState);
								var updatedPost = _Utils_update(
									post,
									{
										comments: A3(
											elm$core$Array$set,
											commentIndex,
											_Utils_update(
												comment,
												{
													markdown: A2(elm$core$Maybe$withDefault, '', updateComment)
												}),
											post.comments)
									});
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											commentState: A3(author$project$Comments$removeEditDraft, postIndex, commentIndex, model.commentState),
											feed: A3(elm$core$Array$set, postIndex, updatedPost, model.feed)
										}),
									elm$core$Platform$Cmd$none);
							} else {
								return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
							}
						} else {
							return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
						}
					default:
						var postIndex = commentsMsg.a;
						var commentIndex = commentsMsg.b;
						var _n6 = A2(elm$core$Array$get, postIndex, model.feed);
						if (_n6.$ === 'Just') {
							var post = _n6.a;
							var _n7 = A2(elm$core$Array$get, commentIndex, post.comments);
							if (_n7.$ === 'Just') {
								var comment = _n7.a;
								var metadata = comment.metadata;
								var updatedPost = _Utils_update(
									post,
									{
										comments: A3(
											elm$core$Array$set,
											commentIndex,
											_Utils_update(
												comment,
												{
													metadata: _Utils_update(
														metadata,
														{isDeleted: true})
												}),
											post.comments)
									});
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											commentState: A3(author$project$Comments$removeEditDraft, postIndex, commentIndex, model.commentState),
											feed: A3(elm$core$Array$set, postIndex, updatedPost, model.feed)
										}),
									elm$core$Platform$Cmd$none);
							} else {
								return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
							}
						} else {
							return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
						}
				}
			case 'NoOp':
				return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							feed: A2(elm$core$Array$push, author$project$Example$examplePost, model.feed)
						}),
					elm$core$Platform$Cmd$none);
		}
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$unwrapSnippet = function (_n0) {
	var declarations = _n0.a;
	return declarations;
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$collectSelectors = function (declarations) {
	collectSelectors:
	while (true) {
		if (!declarations.b) {
			return _List_Nil;
		} else {
			if (declarations.a.$ === 'StyleBlockDeclaration') {
				var _n1 = declarations.a.a;
				var firstSelector = _n1.a;
				var otherSelectors = _n1.b;
				var rest = declarations.b;
				return _Utils_ap(
					A2(elm$core$List$cons, firstSelector, otherSelectors),
					ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$collectSelectors(rest));
			} else {
				var rest = declarations.b;
				var $temp$declarations = rest;
				declarations = $temp$declarations;
				continue collectSelectors;
			}
		}
	}
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$concatDeclarationsAndWarnings = function (declarationsAndWarnings) {
	if (!declarationsAndWarnings.b) {
		return {declarations: _List_Nil, warnings: _List_Nil};
	} else {
		var declarations = declarationsAndWarnings.a.declarations;
		var warnings = declarationsAndWarnings.a.warnings;
		var rest = declarationsAndWarnings.b;
		var result = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$concatDeclarationsAndWarnings(rest);
		return {
			declarations: _Utils_ap(declarations, result.declarations),
			warnings: _Utils_ap(warnings, result.warnings)
		};
	}
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarning = function (_n0) {
	var warnings = _n0.warnings;
	var key = _n0.key;
	var value = _n0.value;
	var important = _n0.important;
	return _Utils_Tuple2(
		warnings,
		{important: important, key: key, value: value});
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$lastDeclaration = function (declarations) {
	lastDeclaration:
	while (true) {
		if (!declarations.b) {
			return elm$core$Maybe$Nothing;
		} else {
			if (!declarations.b.b) {
				var x = declarations.a;
				return elm$core$Maybe$Just(
					_List_fromArray(
						[x]));
			} else {
				var xs = declarations.b;
				var $temp$declarations = xs;
				declarations = $temp$declarations;
				continue lastDeclaration;
			}
		}
	}
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$oneOf = function (maybes) {
	oneOf:
	while (true) {
		if (!maybes.b) {
			return elm$core$Maybe$Nothing;
		} else {
			var maybe = maybes.a;
			var rest = maybes.b;
			if (maybe.$ === 'Nothing') {
				var $temp$maybes = rest;
				maybes = $temp$maybes;
				continue oneOf;
			} else {
				return maybe;
			}
		}
	}
};
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarnings = function (properties) {
	return _Utils_Tuple2(
		A2(
			elm$core$List$concatMap,
			function ($) {
				return $.warnings;
			},
			properties),
		A2(
			elm$core$List$map,
			function (prop) {
				return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarning(prop).b;
			},
			properties));
};
var ThinkAlexandria$css_in_elm$Css$Structure$Viewport = function (a) {
	return {$: 'Viewport', a: a};
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveCounterStyle = function (counterStyleProperties) {
	var _n0 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarnings(counterStyleProperties);
	var warnings = _n0.a;
	var properties = _n0.b;
	return {
		declarations: _List_fromArray(
			[
				ThinkAlexandria$css_in_elm$Css$Structure$Viewport(properties)
			]),
		warnings: warnings
	};
};
var ThinkAlexandria$css_in_elm$Css$Structure$FontFace = function (a) {
	return {$: 'FontFace', a: a};
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveFontFace = function (fontFaceProperties) {
	var _n0 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarnings(fontFaceProperties);
	var warnings = _n0.a;
	var properties = _n0.b;
	return {
		declarations: _List_fromArray(
			[
				ThinkAlexandria$css_in_elm$Css$Structure$FontFace(properties)
			]),
		warnings: warnings
	};
};
var ThinkAlexandria$css_in_elm$Css$Structure$FontFeatureValues = function (a) {
	return {$: 'FontFeatureValues', a: a};
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveFontFeatureValues = function (tuples) {
	var expandTuples = function (tuplesToExpand) {
		if (!tuplesToExpand.b) {
			return _Utils_Tuple2(_List_Nil, _List_Nil);
		} else {
			var _n1 = tuplesToExpand.a;
			var str = _n1.a;
			var tupleProperties = _n1.b;
			var rest = tuplesToExpand.b;
			var _n2 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarnings(tupleProperties);
			var warnings = _n2.a;
			var properties = _n2.b;
			var _n3 = expandTuples(rest);
			var nextWarnings = _n3.a;
			var nextTuples = _n3.b;
			return _Utils_Tuple2(
				_Utils_ap(warnings, nextWarnings),
				A2(
					elm$core$List$cons,
					_Utils_Tuple2(str, properties),
					nextTuples));
		}
	};
	var _n4 = expandTuples(tuples);
	var warnings = _n4.a;
	var newTuples = _n4.b;
	return {
		declarations: _List_fromArray(
			[
				ThinkAlexandria$css_in_elm$Css$Structure$FontFeatureValues(newTuples)
			]),
		warnings: warnings
	};
};
var ThinkAlexandria$css_in_elm$Css$Structure$Keyframes = F2(
	function (a, b) {
		return {$: 'Keyframes', a: a, b: b};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveKeyframes = F2(
	function (str, properties) {
		return {
			declarations: _List_fromArray(
				[
					A2(ThinkAlexandria$css_in_elm$Css$Structure$Keyframes, str, properties)
				]),
			warnings: _List_Nil
		};
	});
var ThinkAlexandria$css_in_elm$Css$Structure$PageRule = F2(
	function (a, b) {
		return {$: 'PageRule', a: a, b: b};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolvePageRule = F2(
	function (str, pageRuleProperties) {
		var _n0 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarnings(pageRuleProperties);
		var warnings = _n0.a;
		var properties = _n0.b;
		return {
			declarations: _List_fromArray(
				[
					A2(ThinkAlexandria$css_in_elm$Css$Structure$PageRule, str, properties)
				]),
			warnings: warnings
		};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveViewport = function (viewportProperties) {
	var _n0 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarnings(viewportProperties);
	var warnings = _n0.a;
	var properties = _n0.b;
	return {
		declarations: _List_fromArray(
			[
				ThinkAlexandria$css_in_elm$Css$Structure$Viewport(properties)
			]),
		warnings: warnings
	};
};
var ThinkAlexandria$css_in_elm$Css$Structure$DocumentRule = F5(
	function (a, b, c, d, e) {
		return {$: 'DocumentRule', a: a, b: b, c: c, d: d, e: e};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toDocumentRule = F5(
	function (str1, str2, str3, str4, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var structureStyleBlock = declaration.a;
			return A5(ThinkAlexandria$css_in_elm$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
		} else {
			return declaration;
		}
	});
var ThinkAlexandria$css_in_elm$Css$Structure$MediaRule = F2(
	function (a, b) {
		return {$: 'MediaRule', a: a, b: b};
	});
var ThinkAlexandria$css_in_elm$Css$Structure$SupportsRule = F2(
	function (a, b) {
		return {$: 'SupportsRule', a: a, b: b};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toMediaRule = F2(
	function (mediaQueries, declaration) {
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var structureStyleBlock = declaration.a;
				return A2(
					ThinkAlexandria$css_in_elm$Css$Structure$MediaRule,
					mediaQueries,
					_List_fromArray(
						[structureStyleBlock]));
			case 'MediaRule':
				var newMediaQueries = declaration.a;
				var structureStyleBlocks = declaration.b;
				return A2(
					ThinkAlexandria$css_in_elm$Css$Structure$MediaRule,
					_Utils_ap(mediaQueries, newMediaQueries),
					structureStyleBlocks);
			case 'SupportsRule':
				var str = declaration.a;
				var declarations = declaration.b;
				return A2(
					ThinkAlexandria$css_in_elm$Css$Structure$SupportsRule,
					str,
					A2(
						elm$core$List$map,
						ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
						declarations));
			case 'DocumentRule':
				var str1 = declaration.a;
				var str2 = declaration.b;
				var str3 = declaration.c;
				var str4 = declaration.d;
				var structureStyleBlock = declaration.e;
				return A5(ThinkAlexandria$css_in_elm$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
			case 'PageRule':
				return declaration;
			case 'FontFace':
				return declaration;
			case 'Keyframes':
				return declaration;
			case 'Viewport':
				return declaration;
			case 'CounterStyle':
				return declaration;
			default:
				return declaration;
		}
	});
var ThinkAlexandria$css_in_elm$Css$Structure$Selector = F3(
	function (a, b, c) {
		return {$: 'Selector', a: a, b: b, c: c};
	});
var ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var ThinkAlexandria$css_in_elm$Css$Structure$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var ThinkAlexandria$css_in_elm$Css$Structure$mapLast = F2(
	function (update, list) {
		if (!list.b) {
			return list;
		} else {
			if (!list.b.b) {
				var only = list.a;
				return _List_fromArray(
					[
						update(only)
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					elm$core$List$cons,
					first,
					A2(ThinkAlexandria$css_in_elm$Css$Structure$mapLast, update, rest));
			}
		}
	});
var ThinkAlexandria$css_in_elm$Css$Structure$withPropertyAppended = F2(
	function (property, _n0) {
		var firstSelector = _n0.a;
		var otherSelectors = _n0.b;
		var properties = _n0.c;
		return A3(
			ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock,
			firstSelector,
			otherSelectors,
			_Utils_ap(
				properties,
				_List_fromArray(
					[property])));
	});
var ThinkAlexandria$css_in_elm$Css$Structure$appendProperty = F2(
	function (property, declarations) {
		if (!declarations.b) {
			return declarations;
		} else {
			if (!declarations.b.b) {
				switch (declarations.a.$) {
					case 'StyleBlockDeclaration':
						var styleBlock = declarations.a.a;
						return _List_fromArray(
							[
								ThinkAlexandria$css_in_elm$Css$Structure$StyleBlockDeclaration(
								A2(ThinkAlexandria$css_in_elm$Css$Structure$withPropertyAppended, property, styleBlock))
							]);
					case 'MediaRule':
						var _n1 = declarations.a;
						var mediaQueries = _n1.a;
						var styleBlocks = _n1.b;
						return _List_fromArray(
							[
								A2(
								ThinkAlexandria$css_in_elm$Css$Structure$MediaRule,
								mediaQueries,
								A2(
									ThinkAlexandria$css_in_elm$Css$Structure$mapLast,
									ThinkAlexandria$css_in_elm$Css$Structure$withPropertyAppended(property),
									styleBlocks))
							]);
					default:
						return declarations;
				}
			} else {
				var first = declarations.a;
				var rest = declarations.b;
				return A2(
					elm$core$List$cons,
					first,
					A2(ThinkAlexandria$css_in_elm$Css$Structure$appendProperty, property, rest));
			}
		}
	});
var ThinkAlexandria$css_in_elm$Css$Structure$appendToLastSelector = F2(
	function (f, styleBlock) {
		if (!styleBlock.b.b) {
			var only = styleBlock.a;
			var properties = styleBlock.c;
			return _List_fromArray(
				[
					A3(ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock, only, _List_Nil, properties),
					A3(
					ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock,
					f(only),
					_List_Nil,
					_List_Nil)
				]);
		} else {
			var first = styleBlock.a;
			var rest = styleBlock.b;
			var properties = styleBlock.c;
			var newRest = A2(elm$core$List$map, f, rest);
			var newFirst = f(first);
			return _List_fromArray(
				[
					A3(ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock, first, rest, properties),
					A3(ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock, newFirst, newRest, _List_Nil)
				]);
		}
	});
var ThinkAlexandria$css_in_elm$Css$Structure$applyPseudoElement = F2(
	function (pseudo, _n0) {
		var sequence = _n0.a;
		var selectors = _n0.b;
		return A3(
			ThinkAlexandria$css_in_elm$Css$Structure$Selector,
			sequence,
			selectors,
			elm$core$Maybe$Just(pseudo));
	});
var ThinkAlexandria$css_in_elm$Css$Structure$appendPseudoElementToLastSelector = F2(
	function (pseudo, styleBlock) {
		return A2(
			ThinkAlexandria$css_in_elm$Css$Structure$appendToLastSelector,
			ThinkAlexandria$css_in_elm$Css$Structure$applyPseudoElement(pseudo),
			styleBlock);
	});
var ThinkAlexandria$css_in_elm$Css$Structure$CustomSelector = F2(
	function (a, b) {
		return {$: 'CustomSelector', a: a, b: b};
	});
var ThinkAlexandria$css_in_elm$Css$Structure$TypeSelectorSequence = F2(
	function (a, b) {
		return {$: 'TypeSelectorSequence', a: a, b: b};
	});
var ThinkAlexandria$css_in_elm$Css$Structure$UniversalSelectorSequence = function (a) {
	return {$: 'UniversalSelectorSequence', a: a};
};
var ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatable = F2(
	function (selector, sequence) {
		switch (sequence.$) {
			case 'TypeSelectorSequence':
				var typeSelector = sequence.a;
				var list = sequence.b;
				return A2(
					ThinkAlexandria$css_in_elm$Css$Structure$TypeSelectorSequence,
					typeSelector,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			case 'UniversalSelectorSequence':
				var list = sequence.a;
				return ThinkAlexandria$css_in_elm$Css$Structure$UniversalSelectorSequence(
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			default:
				var str = sequence.a;
				var list = sequence.b;
				return A2(
					ThinkAlexandria$css_in_elm$Css$Structure$CustomSelector,
					str,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
		}
	});
var ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatableWithCombinator = F2(
	function (selector, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			if (!list.b.b) {
				var _n1 = list.a;
				var combinator = _n1.a;
				var sequence = _n1.b;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						combinator,
						A2(ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatable, selector, sequence))
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					elm$core$List$cons,
					first,
					A2(ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatableWithCombinator, selector, rest));
			}
		}
	});
var ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatableSelector = F2(
	function (repeatableSimpleSelector, selector) {
		if (!selector.b.b) {
			var sequence = selector.a;
			var pseudoElement = selector.c;
			return A3(
				ThinkAlexandria$css_in_elm$Css$Structure$Selector,
				A2(ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatable, repeatableSimpleSelector, sequence),
				_List_Nil,
				pseudoElement);
		} else {
			var firstSelector = selector.a;
			var tuples = selector.b;
			var pseudoElement = selector.c;
			return A3(
				ThinkAlexandria$css_in_elm$Css$Structure$Selector,
				firstSelector,
				A2(ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatableWithCombinator, repeatableSimpleSelector, tuples),
				pseudoElement);
		}
	});
var ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatableToLastSelector = F2(
	function (selector, styleBlock) {
		return A2(
			ThinkAlexandria$css_in_elm$Css$Structure$appendToLastSelector,
			ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatableSelector(selector),
			styleBlock);
	});
var ThinkAlexandria$css_in_elm$Css$Structure$concatMapLastStyleBlock = F2(
	function (update, declarations) {
		_n0$12:
		while (true) {
			if (!declarations.b) {
				return declarations;
			} else {
				if (!declarations.b.b) {
					switch (declarations.a.$) {
						case 'StyleBlockDeclaration':
							var styleBlock = declarations.a.a;
							return A2(
								elm$core$List$map,
								ThinkAlexandria$css_in_elm$Css$Structure$StyleBlockDeclaration,
								update(styleBlock));
						case 'MediaRule':
							if (declarations.a.b.b) {
								if (!declarations.a.b.b.b) {
									var _n1 = declarations.a;
									var mediaQueries = _n1.a;
									var _n2 = _n1.b;
									var styleBlock = _n2.a;
									return _List_fromArray(
										[
											A2(
											ThinkAlexandria$css_in_elm$Css$Structure$MediaRule,
											mediaQueries,
											update(styleBlock))
										]);
								} else {
									var _n3 = declarations.a;
									var mediaQueries = _n3.a;
									var _n4 = _n3.b;
									var first = _n4.a;
									var rest = _n4.b;
									var _n5 = A2(
										ThinkAlexandria$css_in_elm$Css$Structure$concatMapLastStyleBlock,
										update,
										_List_fromArray(
											[
												A2(ThinkAlexandria$css_in_elm$Css$Structure$MediaRule, mediaQueries, rest)
											]));
									if ((_n5.b && (_n5.a.$ === 'MediaRule')) && (!_n5.b.b)) {
										var _n6 = _n5.a;
										var newMediaQueries = _n6.a;
										var newStyleBlocks = _n6.b;
										return _List_fromArray(
											[
												A2(
												ThinkAlexandria$css_in_elm$Css$Structure$MediaRule,
												newMediaQueries,
												A2(elm$core$List$cons, first, newStyleBlocks))
											]);
									} else {
										var unhandledDeclarations = _n5;
										return unhandledDeclarations;
									}
								}
							} else {
								break _n0$12;
							}
						case 'SupportsRule':
							var _n7 = declarations.a;
							var str = _n7.a;
							var nestedDeclarations = _n7.b;
							return _List_fromArray(
								[
									A2(
									ThinkAlexandria$css_in_elm$Css$Structure$SupportsRule,
									str,
									A2(ThinkAlexandria$css_in_elm$Css$Structure$concatMapLastStyleBlock, update, nestedDeclarations))
								]);
						case 'DocumentRule':
							var _n8 = declarations.a;
							var str1 = _n8.a;
							var str2 = _n8.b;
							var str3 = _n8.c;
							var str4 = _n8.d;
							var styleBlock = _n8.e;
							return A2(
								elm$core$List$map,
								A4(ThinkAlexandria$css_in_elm$Css$Structure$DocumentRule, str1, str2, str3, str4),
								update(styleBlock));
						case 'PageRule':
							var _n9 = declarations.a;
							return declarations;
						case 'FontFace':
							return declarations;
						case 'Keyframes':
							var _n10 = declarations.a;
							return declarations;
						case 'Viewport':
							return declarations;
						case 'CounterStyle':
							return declarations;
						default:
							return declarations;
					}
				} else {
					break _n0$12;
				}
			}
		}
		var first = declarations.a;
		var rest = declarations.b;
		return A2(
			elm$core$List$cons,
			first,
			A2(ThinkAlexandria$css_in_elm$Css$Structure$concatMapLastStyleBlock, update, rest));
	});
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(xs);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyNestedStylesToLast = F4(
	function (nestedStyles, rest, f, declarations) {
		var withoutParent = function (decls) {
			return A2(
				elm$core$Maybe$withDefault,
				_List_Nil,
				elm$core$List$tail(decls));
		};
		var nextResult = A2(
			ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles,
			rest,
			A2(
				elm$core$Maybe$withDefault,
				_List_Nil,
				ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		var newDeclarations = function () {
			var _n19 = _Utils_Tuple2(
				elm$core$List$head(nextResult.declarations),
				elm$core$List$head(
					elm$core$List$reverse(declarations)));
			if ((_n19.a.$ === 'Just') && (_n19.b.$ === 'Just')) {
				var nextResultParent = _n19.a.a;
				var originalParent = _n19.b.a;
				return _Utils_ap(
					elm$core$List$reverse(
						A2(
							elm$core$Maybe$withDefault,
							_List_Nil,
							elm$core$List$tail(
								elm$core$List$reverse(declarations)))),
					_List_fromArray(
						[
							(!_Utils_eq(originalParent, nextResultParent)) ? nextResultParent : originalParent
						]));
			} else {
				return declarations;
			}
		}();
		var handleInitial = function (declarationsAndWarnings) {
			var result = A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles, nestedStyles, declarationsAndWarnings.declarations);
			return {
				declarations: result.declarations,
				warnings: _Utils_ap(declarationsAndWarnings.warnings, result.warnings)
			};
		};
		var insertStylesToNestedDecl = function (lastDecl) {
			return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$concatDeclarationsAndWarnings(
				A2(
					ThinkAlexandria$css_in_elm$Css$Structure$mapLast,
					handleInitial,
					A2(
						elm$core$List$map,
						function (declaration) {
							return {
								declarations: _List_fromArray(
									[declaration]),
								warnings: _List_Nil
							};
						},
						A2(ThinkAlexandria$css_in_elm$Css$Structure$concatMapLastStyleBlock, f, lastDecl))));
		};
		var initialResult = A2(
			elm$core$Maybe$withDefault,
			{declarations: _List_Nil, warnings: _List_Nil},
			A2(
				elm$core$Maybe$map,
				insertStylesToNestedDecl,
				ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		return {
			declarations: _Utils_ap(
				newDeclarations,
				_Utils_ap(
					withoutParent(initialResult.declarations),
					withoutParent(nextResult.declarations))),
			warnings: _Utils_ap(initialResult.warnings, nextResult.warnings)
		};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles = F2(
	function (styles, declarations) {
		if (!styles.b) {
			return {declarations: declarations, warnings: _List_Nil};
		} else {
			switch (styles.a.$) {
				case 'AppendProperty':
					var propertyToAppend = styles.a.a;
					var rest = styles.b;
					var _n8 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extractWarning(propertyToAppend);
					var warnings = _n8.a;
					var property = _n8.b;
					var result = A2(
						ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2(ThinkAlexandria$css_in_elm$Css$Structure$appendProperty, property, declarations));
					return {
						declarations: result.declarations,
						warnings: _Utils_ap(warnings, result.warnings)
					};
				case 'ExtendSelector':
					var _n9 = styles.a;
					var selector = _n9.a;
					var nestedStyles = _n9.b;
					var rest = styles.b;
					return A4(
						ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						ThinkAlexandria$css_in_elm$Css$Structure$appendRepeatableToLastSelector(selector),
						declarations);
				case 'NestSnippet':
					var _n10 = styles.a;
					var selectorCombinator = _n10.a;
					var nestSnippets = _n10.b;
					var rest = styles.b;
					var chain = F2(
						function (_n14, _n15) {
							var originalSequence = _n14.a;
							var originalTuples = _n14.b;
							var originalPseudoElement = _n14.c;
							var newSequence = _n15.a;
							var newTuples = _n15.b;
							var newPseudoElement = _n15.c;
							return A3(
								ThinkAlexandria$css_in_elm$Css$Structure$Selector,
								originalSequence,
								_Utils_ap(
									originalTuples,
									A2(
										elm$core$List$cons,
										_Utils_Tuple2(selectorCombinator, newSequence),
										newTuples)),
								ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$oneOf(
									_List_fromArray(
										[newPseudoElement, originalPseudoElement])));
						});
					var expandDeclaration = function (declaration) {
						switch (declaration.$) {
							case 'StyleBlockDeclaration':
								var _n12 = declaration.a;
								var firstSelector = _n12.a;
								var otherSelectors = _n12.b;
								var nestedStyles = _n12.c;
								var newSelectors = A2(
									elm$core$List$concatMap,
									function (originalSelector) {
										return A2(
											elm$core$List$map,
											chain(originalSelector),
											A2(elm$core$List$cons, firstSelector, otherSelectors));
									},
									ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$collectSelectors(declarations));
								var newDeclarations = function () {
									if (!newSelectors.b) {
										return _List_Nil;
									} else {
										var first = newSelectors.a;
										var remaining = newSelectors.b;
										return _List_fromArray(
											[
												ThinkAlexandria$css_in_elm$Css$Structure$StyleBlockDeclaration(
												A3(ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock, first, remaining, _List_Nil))
											]);
									}
								}();
								return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$concatDeclarationsAndWarnings(
									_List_fromArray(
										[
											A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles, nestedStyles, newDeclarations)
										]));
							case 'MediaRule':
								var mediaQueries = declaration.a;
								var styleBlocks = declaration.b;
								return A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
							case 'SupportsRule':
								var str = declaration.a;
								var snippets = declaration.b;
								return A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveSupportsRule, str, snippets);
							case 'DocumentRule':
								var str1 = declaration.a;
								var str2 = declaration.b;
								var str3 = declaration.c;
								var str4 = declaration.d;
								var styleBlock = declaration.e;
								return A5(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveDocumentRule, str1, str2, str3, str4, styleBlock);
							case 'PageRule':
								var str = declaration.a;
								var pageRuleProperties = declaration.b;
								return A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolvePageRule, str, pageRuleProperties);
							case 'FontFace':
								var fontFaceProperties = declaration.a;
								return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveFontFace(fontFaceProperties);
							case 'Keyframes':
								var str = declaration.a;
								var properties = declaration.b;
								return A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveKeyframes, str, properties);
							case 'Viewport':
								var viewportProperties = declaration.a;
								return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveViewport(viewportProperties);
							case 'CounterStyle':
								var counterStyleProperties = declaration.a;
								return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveCounterStyle(counterStyleProperties);
							default:
								var tuples = declaration.a;
								return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
						}
					};
					return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$concatDeclarationsAndWarnings(
						_Utils_ap(
							_List_fromArray(
								[
									A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles, rest, declarations)
								]),
							A2(
								elm$core$List$map,
								expandDeclaration,
								A2(elm$core$List$concatMap, ThinkAlexandria$css_in_elm$Css$Preprocess$unwrapSnippet, nestSnippets))));
				case 'WithPseudoElement':
					var _n16 = styles.a;
					var pseudoElement = _n16.a;
					var nestedStyles = _n16.b;
					var rest = styles.b;
					return A4(
						ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						ThinkAlexandria$css_in_elm$Css$Structure$appendPseudoElementToLastSelector(pseudoElement),
						declarations);
				case 'WithMedia':
					var _n17 = styles.a;
					var mediaQueries = _n17.a;
					var nestedStyles = _n17.b;
					var rest = styles.b;
					var newDeclarations = function () {
						var _n18 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$collectSelectors(declarations);
						if (!_n18.b) {
							return _List_Nil;
						} else {
							var firstSelector = _n18.a;
							var otherSelectors = _n18.b;
							return _List_fromArray(
								[
									A2(
									ThinkAlexandria$css_in_elm$Css$Structure$MediaRule,
									mediaQueries,
									_List_fromArray(
										[
											A3(ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil)
										]))
								]);
						}
					}();
					return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$concatDeclarationsAndWarnings(
						_List_fromArray(
							[
								A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles, rest, declarations),
								A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles, nestedStyles, newDeclarations)
							]));
				default:
					var otherStyles = styles.a.a;
					var rest = styles.b;
					return A2(
						ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles,
						_Utils_ap(otherStyles, rest),
						declarations);
			}
		}
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$expandStyleBlock = function (_n6) {
	var firstSelector = _n6.a;
	var otherSelectors = _n6.b;
	var styles = _n6.c;
	return A2(
		ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$applyStyles,
		styles,
		_List_fromArray(
			[
				ThinkAlexandria$css_in_elm$Css$Structure$StyleBlockDeclaration(
				A3(ThinkAlexandria$css_in_elm$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil))
			]));
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extract = function (snippetDeclarations) {
	if (!snippetDeclarations.b) {
		return {declarations: _List_Nil, warnings: _List_Nil};
	} else {
		var first = snippetDeclarations.a;
		var rest = snippetDeclarations.b;
		var nextResult = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extract(rest);
		var _n5 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toDeclarations(first);
		var declarations = _n5.declarations;
		var warnings = _n5.warnings;
		return {
			declarations: _Utils_ap(declarations, nextResult.declarations),
			warnings: _Utils_ap(warnings, nextResult.warnings)
		};
	}
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveDocumentRule = F5(
	function (str1, str2, str3, str4, styleBlock) {
		var _n3 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$expandStyleBlock(styleBlock);
		var declarations = _n3.declarations;
		var warnings = _n3.warnings;
		return {
			declarations: A2(
				elm$core$List$map,
				A4(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
				declarations),
			warnings: warnings
		};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveMediaRule = F2(
	function (mediaQueries, styleBlocks) {
		var handleStyleBlock = function (styleBlock) {
			var _n2 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$expandStyleBlock(styleBlock);
			var declarations = _n2.declarations;
			var warnings = _n2.warnings;
			return {
				declarations: A2(
					elm$core$List$map,
					ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
					declarations),
				warnings: warnings
			};
		};
		var results = A2(elm$core$List$map, handleStyleBlock, styleBlocks);
		return {
			declarations: A2(
				elm$core$List$concatMap,
				function ($) {
					return $.declarations;
				},
				results),
			warnings: A2(
				elm$core$List$concatMap,
				function ($) {
					return $.warnings;
				},
				results)
		};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveSupportsRule = F2(
	function (str, snippets) {
		var _n1 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extract(
			A2(elm$core$List$concatMap, ThinkAlexandria$css_in_elm$Css$Preprocess$unwrapSnippet, snippets));
		var declarations = _n1.declarations;
		var warnings = _n1.warnings;
		return {
			declarations: _List_fromArray(
				[
					A2(ThinkAlexandria$css_in_elm$Css$Structure$SupportsRule, str, declarations)
				]),
			warnings: warnings
		};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toDeclarations = function (snippetDeclaration) {
	switch (snippetDeclaration.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = snippetDeclaration.a;
			return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$expandStyleBlock(styleBlock);
		case 'MediaRule':
			var mediaQueries = snippetDeclaration.a;
			var styleBlocks = snippetDeclaration.b;
			return A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
		case 'SupportsRule':
			var str = snippetDeclaration.a;
			var snippets = snippetDeclaration.b;
			return A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveSupportsRule, str, snippets);
		case 'DocumentRule':
			var str1 = snippetDeclaration.a;
			var str2 = snippetDeclaration.b;
			var str3 = snippetDeclaration.c;
			var str4 = snippetDeclaration.d;
			var styleBlock = snippetDeclaration.e;
			return A5(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveDocumentRule, str1, str2, str3, str4, styleBlock);
		case 'PageRule':
			var str = snippetDeclaration.a;
			var pageRuleProperties = snippetDeclaration.b;
			return A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolvePageRule, str, pageRuleProperties);
		case 'FontFace':
			var fontFaceProperties = snippetDeclaration.a;
			return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveFontFace(fontFaceProperties);
		case 'Keyframes':
			var str = snippetDeclaration.a;
			var properties = snippetDeclaration.b;
			return A2(ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveKeyframes, str, properties);
		case 'Viewport':
			var viewportProperties = snippetDeclaration.a;
			return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveViewport(viewportProperties);
		case 'CounterStyle':
			var counterStyleProperties = snippetDeclaration.a;
			return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveCounterStyle(counterStyleProperties);
		default:
			var tuples = snippetDeclaration.a;
			return ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
	}
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toStructure = function (_n0) {
	var charset = _n0.charset;
	var imports = _n0.imports;
	var namespaces = _n0.namespaces;
	var snippets = _n0.snippets;
	var _n1 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$extract(
		A2(elm$core$List$concatMap, ThinkAlexandria$css_in_elm$Css$Preprocess$unwrapSnippet, snippets));
	var warnings = _n1.warnings;
	var declarations = _n1.declarations;
	return _Utils_Tuple2(
		{charset: charset, declarations: declarations, imports: imports, namespaces: namespaces},
		warnings);
};
var elm$core$Basics$not = _Basics_not;
var elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			elm$core$List$any,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, isOkay),
			list);
	});
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations = function (declarations) {
	dropEmptyDeclarations:
	while (true) {
		if (!declarations.b) {
			return _List_Nil;
		} else {
			switch (declarations.a.$) {
				case 'StyleBlockDeclaration':
					var declaration = declarations.a;
					var _n1 = declaration.a;
					var properties = _n1.c;
					var rest = declarations.b;
					if (elm$core$List$isEmpty(properties)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
				case 'MediaRule':
					var declaration = declarations.a;
					var styleBlocks = declaration.b;
					var rest = declarations.b;
					if (A2(
						elm$core$List$all,
						function (_n2) {
							var properties = _n2.c;
							return elm$core$List$isEmpty(properties);
						},
						styleBlocks)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
				case 'SupportsRule':
					var declaration = declarations.a;
					var otherDeclarations = declaration.b;
					var rest = declarations.b;
					if (elm$core$List$isEmpty(otherDeclarations)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
				case 'DocumentRule':
					var declaration = declarations.a;
					var rest = declarations.b;
					return A2(
						elm$core$List$cons,
						declaration,
						ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
				case 'PageRule':
					var declaration = declarations.a;
					var properties = declaration.b;
					var rest = declarations.b;
					if (elm$core$List$isEmpty(properties)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
				case 'FontFace':
					var declaration = declarations.a;
					var properties = declaration.a;
					var rest = declarations.b;
					if (elm$core$List$isEmpty(properties)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
				case 'Keyframes':
					var declaration = declarations.a;
					var properties = declaration.b;
					var rest = declarations.b;
					if (elm$core$List$isEmpty(properties)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
				case 'Viewport':
					var declaration = declarations.a;
					var properties = declaration.a;
					var rest = declarations.b;
					if (elm$core$List$isEmpty(properties)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
				case 'CounterStyle':
					var declaration = declarations.a;
					var properties = declaration.a;
					var rest = declarations.b;
					if (elm$core$List$isEmpty(properties)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
				default:
					var declaration = declarations.a;
					var tuples = declaration.a;
					var rest = declarations.b;
					if (A2(
						elm$core$List$all,
						function (_n3) {
							var properties = _n3.b;
							return elm$core$List$isEmpty(properties);
						},
						tuples)) {
						var $temp$declarations = rest;
						declarations = $temp$declarations;
						continue dropEmptyDeclarations;
					} else {
						return A2(
							elm$core$List$cons,
							declaration,
							ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(rest));
					}
			}
		}
	}
};
var ThinkAlexandria$css_in_elm$Css$Structure$dropEmpty = function (_n0) {
	var charset = _n0.charset;
	var imports = _n0.imports;
	var namespaces = _n0.namespaces;
	var declarations = _n0.declarations;
	return {
		charset: charset,
		declarations: ThinkAlexandria$css_in_elm$Css$Structure$dropEmptyDeclarations(declarations),
		imports: imports,
		namespaces: namespaces
	};
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$charsetToString = function (charset) {
	return A2(
		elm$core$Maybe$withDefault,
		'',
		A2(
			elm$core$Maybe$map,
			function (str) {
				return '@charset \"' + (str + '\"');
			},
			charset));
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$mediaExpressionToString = function (expression) {
	return '(' + (expression.feature + (A2(
		elm$core$Maybe$withDefault,
		'',
		A2(
			elm$core$Maybe$map,
			elm$core$Basics$append(': '),
			expression.value)) + ')'));
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$mediaTypeToString = function (mediaType) {
	switch (mediaType.$) {
		case 'Print':
			return 'print';
		case 'Screen':
			return 'screen';
		default:
			return 'speech';
	}
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$mediaQueryToString = function (mediaQuery) {
	var prefixWith = F3(
		function (str, mediaType, expressions) {
			return str + (' ' + A2(
				elm$core$String$join,
				' and ',
				A2(
					elm$core$List$cons,
					ThinkAlexandria$css_in_elm$Css$Structure$Output$mediaTypeToString(mediaType),
					A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$mediaExpressionToString, expressions))));
		});
	switch (mediaQuery.$) {
		case 'AllQuery':
			var expressions = mediaQuery.a;
			return A2(
				elm$core$String$join,
				' and ',
				A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$mediaExpressionToString, expressions));
		case 'OnlyQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'only', mediaType, expressions);
		case 'NotQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'not', mediaType, expressions);
		default:
			var str = mediaQuery.a;
			return str;
	}
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$importToString = function (_n0) {
	var name = _n0.a;
	var mediaQueries = _n0.b;
	return '@import \"' + (name + ('[' + (A2(
		elm$core$String$join,
		',',
		A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$mediaQueryToString, mediaQueries)) + ']\"')));
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$namespaceToString = function (_n0) {
	var prefix = _n0.a;
	var str = _n0.b;
	return '@namespace ' + (prefix + ('\"' + (str + '\"')));
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$spaceIndent = '    ';
var ThinkAlexandria$css_in_elm$Css$Structure$Output$indent = function (str) {
	return _Utils_ap(ThinkAlexandria$css_in_elm$Css$Structure$Output$spaceIndent, str);
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$noIndent = '';
var ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintProperty = function (_n0) {
	var key = _n0.key;
	var value = _n0.value;
	var important = _n0.important;
	var suffix = important ? ' !important;' : ';';
	return key + (': ' + (value + suffix));
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintProperties = function (properties) {
	return A2(
		elm$core$String$join,
		'\n',
		A2(
			elm$core$List$map,
			A2(elm$core$Basics$composeL, ThinkAlexandria$css_in_elm$Css$Structure$Output$indent, ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintProperty),
			properties));
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$pseudoElementToString = function (_n0) {
	var str = _n0.a;
	return '::' + str;
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$combinatorToString = function (combinator) {
	switch (combinator.$) {
		case 'AdjacentSibling':
			return '+';
		case 'GeneralSibling':
			return '~';
		case 'Child':
			return '>';
		default:
			return '';
	}
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$repeatableSimpleSelectorToString = function (repeatableSimpleSelector) {
	switch (repeatableSimpleSelector.$) {
		case 'ClassSelector':
			var str = repeatableSimpleSelector.a;
			return '.' + str;
		case 'IdSelector':
			var str = repeatableSimpleSelector.a;
			return '#' + str;
		default:
			var str = repeatableSimpleSelector.a;
			return ':' + str;
	}
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$simpleSelectorSequenceToString = function (simpleSelectorSequence) {
	switch (simpleSelectorSequence.$) {
		case 'TypeSelectorSequence':
			var str = simpleSelectorSequence.a.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$cons,
					str,
					A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors)));
		case 'UniversalSelectorSequence':
			var repeatableSimpleSelectors = simpleSelectorSequence.a;
			return elm$core$List$isEmpty(repeatableSimpleSelectors) ? '*' : A2(
				elm$core$String$join,
				'',
				A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors));
		default:
			var str = simpleSelectorSequence.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return A2(
				elm$core$String$join,
				'',
				A2(
					elm$core$List$cons,
					str,
					A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors)));
	}
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$selectorChainToString = function (_n0) {
	var combinator = _n0.a;
	var sequence = _n0.b;
	return A2(
		elm$core$String$join,
		' ',
		_List_fromArray(
			[
				ThinkAlexandria$css_in_elm$Css$Structure$Output$combinatorToString(combinator),
				ThinkAlexandria$css_in_elm$Css$Structure$Output$simpleSelectorSequenceToString(sequence)
			]));
};
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var ThinkAlexandria$css_in_elm$Css$Structure$Output$selectorToString = function (_n0) {
	var simpleSelectorSequence = _n0.a;
	var chain = _n0.b;
	var pseudoElement = _n0.c;
	var segments = _Utils_ap(
		_List_fromArray(
			[
				ThinkAlexandria$css_in_elm$Css$Structure$Output$simpleSelectorSequenceToString(simpleSelectorSequence)
			]),
		A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$selectorChainToString, chain));
	var pseudoElementsString = A2(
		elm$core$String$join,
		'',
		_List_fromArray(
			[
				A2(
				elm$core$Maybe$withDefault,
				'',
				A2(elm$core$Maybe$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$pseudoElementToString, pseudoElement))
			]));
	return function (a) {
		return _Utils_ap(a, pseudoElementsString);
	}(
		A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$filter,
				A2(elm$core$Basics$composeL, elm$core$Basics$not, elm$core$String$isEmpty),
				segments)));
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintStyleBlock = F2(
	function (indentLevel, _n0) {
		var firstSelector = _n0.a;
		var otherSelectors = _n0.b;
		var properties = _n0.c;
		var selectorStr = A2(
			elm$core$String$join,
			', ',
			A2(
				elm$core$List$map,
				ThinkAlexandria$css_in_elm$Css$Structure$Output$selectorToString,
				A2(elm$core$List$cons, firstSelector, otherSelectors)));
		return A2(
			elm$core$String$join,
			'',
			_List_fromArray(
				[
					selectorStr,
					' {\n',
					indentLevel,
					ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintProperties(properties),
					'\n',
					indentLevel,
					'}'
				]));
	});
var ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintDeclaration = function (declaration) {
	switch (declaration.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = declaration.a;
			return A2(ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintStyleBlock, ThinkAlexandria$css_in_elm$Css$Structure$Output$noIndent, styleBlock);
		case 'MediaRule':
			var mediaQueries = declaration.a;
			var styleBlocks = declaration.b;
			var query = A2(
				elm$core$String$join,
				',\n',
				A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$mediaQueryToString, mediaQueries));
			var blocks = A2(
				elm$core$String$join,
				'\n\n',
				A2(
					elm$core$List$map,
					A2(
						elm$core$Basics$composeL,
						ThinkAlexandria$css_in_elm$Css$Structure$Output$indent,
						ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintStyleBlock(ThinkAlexandria$css_in_elm$Css$Structure$Output$spaceIndent)),
					styleBlocks));
			return '@media ' + (query + (' {\n' + (blocks + '\n}')));
		default:
			return 'TODO not yet implemented :x';
	}
};
var ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrint = function (_n0) {
	var charset = _n0.charset;
	var imports = _n0.imports;
	var namespaces = _n0.namespaces;
	var declarations = _n0.declarations;
	return A2(
		elm$core$String$join,
		'\n\n',
		A2(
			elm$core$List$filter,
			A2(elm$core$Basics$composeL, elm$core$Basics$not, elm$core$String$isEmpty),
			_List_fromArray(
				[
					ThinkAlexandria$css_in_elm$Css$Structure$Output$charsetToString(charset),
					A2(
					elm$core$String$join,
					'\n',
					A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$importToString, imports)),
					A2(
					elm$core$String$join,
					'\n',
					A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$namespaceToString, namespaces)),
					A2(
					elm$core$String$join,
					'\n\n',
					A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrintDeclaration, declarations))
				])));
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$compile1 = function (sheet) {
	var _n0 = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$toStructure(sheet);
	var structureStylesheet = _n0.a;
	var warnings = _n0.b;
	return {
		css: ThinkAlexandria$css_in_elm$Css$Structure$Output$prettyPrint(
			ThinkAlexandria$css_in_elm$Css$Structure$dropEmpty(structureStylesheet)),
		warnings: warnings
	};
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$compile = function (styles) {
	var results = A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$compile1, styles);
	return {
		css: A2(
			elm$core$String$join,
			'\n\n',
			A2(
				elm$core$List$map,
				function ($) {
					return $.css;
				},
				results)),
		warnings: A2(
			elm$core$List$concatMap,
			function ($) {
				return $.warnings;
			},
			results)
	};
};
var ThinkAlexandria$css_in_elm$Css$compile = ThinkAlexandria$css_in_elm$Css$Preprocess$Resolve$compile;
var ThinkAlexandria$css_in_elm$Css$Preprocess$stylesheet = function (snippets) {
	return {charset: elm$core$Maybe$Nothing, imports: _List_Nil, namespaces: _List_Nil, snippets: snippets};
};
var ThinkAlexandria$css_in_elm$Css$stylesheet = ThinkAlexandria$css_in_elm$Css$Preprocess$stylesheet;
var author$project$Comments$Css$CommentEditor = {$: 'CommentEditor'};
var author$project$Comments$Css$CommentEditorBody = {$: 'CommentEditorBody'};
var author$project$Comments$Css$CommentEditorButton = {$: 'CommentEditorButton'};
var author$project$Comments$Css$CommentEditorFooter = {$: 'CommentEditorFooter'};
var author$project$Comments$Css$CommentEditorHeader = {$: 'CommentEditorHeader'};
var author$project$Comments$Css$CommentEditorToolbarButton = {$: 'CommentEditorToolbarButton'};
var author$project$Comments$Css$CommentSpacer = {$: 'CommentSpacer'};
var author$project$Comments$Css$CommentTextInput = {$: 'CommentTextInput'};
var author$project$Comments$Css$HorizontalTab = {$: 'HorizontalTab'};
var author$project$Comments$Css$HorizontalTabCommentEditorSelected = {$: 'HorizontalTabCommentEditorSelected'};
var author$project$Comments$Css$HorizontalTabNav = {$: 'HorizontalTabNav'};
var author$project$Comments$Css$MarkdownBody = {$: 'MarkdownBody'};
var author$project$Comments$Css$defaultCssConfig = {
	body: author$project$Comments$Css$CommentEditorBody,
	button: author$project$Comments$Css$CommentEditorButton,
	editor: author$project$Comments$Css$CommentEditor,
	footer: author$project$Comments$Css$CommentEditorFooter,
	header: author$project$Comments$Css$CommentEditorHeader,
	horizontalTab: author$project$Comments$Css$HorizontalTab,
	horizontalTabNav: author$project$Comments$Css$HorizontalTabNav,
	horizontalTabSelected: author$project$Comments$Css$HorizontalTabCommentEditorSelected,
	markdownBody: author$project$Comments$Css$MarkdownBody,
	spacer: author$project$Comments$Css$CommentSpacer,
	textInput: author$project$Comments$Css$CommentTextInput,
	toString: function (c) {
		switch (c.$) {
			case 'CommentEditor':
				return 'CommentEditor';
			case 'CommentEditorHeader':
				return 'CommentEditorHeader';
			case 'CommentEditorToolbarButton':
				return 'CommentEditorToolbarButton';
			case 'CommentEditorBody':
				return 'CommentEditorBody';
			case 'MarkdownBody':
				return 'MarkdownBody';
			case 'CommentTextInput':
				return 'CommentTextInput';
			case 'CommentEditorFooter':
				return 'CommentEditorFooter';
			case 'CommentEditorButton':
				return 'CommentEditorButton';
			case 'CommentSpacer':
				return 'CommentSpacer';
			case 'HorizontalTabNav':
				return 'HorizontalTabNav';
			case 'HorizontalTab':
				return 'HorizontalTab';
			default:
				return 'HorizontalTabCommentEditorSelected';
		}
	},
	toolbarButton: author$project$Comments$Css$CommentEditorToolbarButton
};
var elm$core$String$cons = _String_cons;
var elm$core$String$startsWith = _String_startsWith;
var ThinkAlexandria$css_in_elm$Css$withPrecedingHash = function (str) {
	return A2(elm$core$String$startsWith, '#', str) ? str : A2(
		elm$core$String$cons,
		_Utils_chr('#'),
		str);
};
var ThinkAlexandria$css_in_elm$Css$Structure$Compatible = {$: 'Compatible'};
var elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var ThinkAlexandria$css_in_elm$Css$erroneousHex = function (str) {
	return {
		alpha: 1,
		blue: 0,
		color: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
		green: 0,
		red: 0,
		value: ThinkAlexandria$css_in_elm$Css$withPrecedingHash(str),
		warnings: elm$core$List$singleton(
			A2(
				elm$core$String$join,
				' ',
				_List_fromArray(
					['Hex color strings must contain exactly 3, 4, 6, or 8 hexadecimal digits, optionally preceded by \"#\".', str, 'is an invalid hex color string.', 'Please see: https://drafts.csswg.org/css-color/#hex-notation'])))
	};
};
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$core$String$fromList = _String_fromList;
var elm$core$String$toLower = _String_toLower;
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$core$String$length = _String_length;
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var elm$core$Basics$pow = _Basics_pow;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char.valueOf()) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2(elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2(elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return elm$core$Result$Err(
							elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var rtfeldman$elm_hex$Hex$fromString = function (str) {
	if (elm$core$String$isEmpty(str)) {
		return elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2(elm$core$String$startsWith, '-', str)) {
				var list = A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					elm$core$List$tail(
						elm$core$String$toList(str)));
				return A2(
					elm$core$Result$map,
					elm$core$Basics$negate,
					A3(
						rtfeldman$elm_hex$Hex$fromStringHelp,
						elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					rtfeldman$elm_hex$Hex$fromStringHelp,
					elm$core$String$length(str) - 1,
					elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2(elm$core$Result$mapError, formatError, result);
	}
};
var ThinkAlexandria$css_in_elm$Css$validHex = F5(
	function (str, _n0, _n1, _n2, _n3) {
		var r1 = _n0.a;
		var r2 = _n0.b;
		var g1 = _n1.a;
		var g2 = _n1.b;
		var b1 = _n2.a;
		var b2 = _n2.b;
		var a1 = _n3.a;
		var a2 = _n3.b;
		var toResult = A2(
			elm$core$Basics$composeR,
			elm$core$String$fromList,
			A2(elm$core$Basics$composeR, elm$core$String$toLower, rtfeldman$elm_hex$Hex$fromString));
		var results = _List_fromArray(
			[
				toResult(
				_List_fromArray(
					[r1, r2])),
				toResult(
				_List_fromArray(
					[g1, g2])),
				toResult(
				_List_fromArray(
					[b1, b2])),
				toResult(
				_List_fromArray(
					[a1, a2]))
			]);
		if ((((((((results.b && (results.a.$ === 'Ok')) && results.b.b) && (results.b.a.$ === 'Ok')) && results.b.b.b) && (results.b.b.a.$ === 'Ok')) && results.b.b.b.b) && (results.b.b.b.a.$ === 'Ok')) && (!results.b.b.b.b.b)) {
			var red = results.a.a;
			var _n5 = results.b;
			var green = _n5.a.a;
			var _n6 = _n5.b;
			var blue = _n6.a.a;
			var _n7 = _n6.b;
			var alpha = _n7.a.a;
			return {
				alpha: alpha / 255,
				blue: blue,
				color: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
				green: green,
				red: red,
				value: ThinkAlexandria$css_in_elm$Css$withPrecedingHash(str),
				warnings: _List_Nil
			};
		} else {
			return ThinkAlexandria$css_in_elm$Css$erroneousHex(str);
		}
	});
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var ThinkAlexandria$css_in_elm$Css$hex = function (str) {
	var withoutHash = A2(elm$core$String$startsWith, '#', str) ? A2(elm$core$String$dropLeft, 1, str) : str;
	var _n0 = elm$core$String$toList(withoutHash);
	_n0$4:
	while (true) {
		if ((_n0.b && _n0.b.b) && _n0.b.b.b) {
			if (!_n0.b.b.b.b) {
				var r = _n0.a;
				var _n1 = _n0.b;
				var g = _n1.a;
				var _n2 = _n1.b;
				var b = _n2.a;
				return A5(
					ThinkAlexandria$css_in_elm$Css$validHex,
					str,
					_Utils_Tuple2(r, r),
					_Utils_Tuple2(g, g),
					_Utils_Tuple2(b, b),
					_Utils_Tuple2(
						_Utils_chr('f'),
						_Utils_chr('f')));
			} else {
				if (!_n0.b.b.b.b.b) {
					var r = _n0.a;
					var _n3 = _n0.b;
					var g = _n3.a;
					var _n4 = _n3.b;
					var b = _n4.a;
					var _n5 = _n4.b;
					var a = _n5.a;
					return A5(
						ThinkAlexandria$css_in_elm$Css$validHex,
						str,
						_Utils_Tuple2(r, r),
						_Utils_Tuple2(g, g),
						_Utils_Tuple2(b, b),
						_Utils_Tuple2(a, a));
				} else {
					if (_n0.b.b.b.b.b.b) {
						if (!_n0.b.b.b.b.b.b.b) {
							var r1 = _n0.a;
							var _n6 = _n0.b;
							var r2 = _n6.a;
							var _n7 = _n6.b;
							var g1 = _n7.a;
							var _n8 = _n7.b;
							var g2 = _n8.a;
							var _n9 = _n8.b;
							var b1 = _n9.a;
							var _n10 = _n9.b;
							var b2 = _n10.a;
							return A5(
								ThinkAlexandria$css_in_elm$Css$validHex,
								str,
								_Utils_Tuple2(r1, r2),
								_Utils_Tuple2(g1, g2),
								_Utils_Tuple2(b1, b2),
								_Utils_Tuple2(
									_Utils_chr('f'),
									_Utils_chr('f')));
						} else {
							if (_n0.b.b.b.b.b.b.b.b && (!_n0.b.b.b.b.b.b.b.b.b)) {
								var r1 = _n0.a;
								var _n11 = _n0.b;
								var r2 = _n11.a;
								var _n12 = _n11.b;
								var g1 = _n12.a;
								var _n13 = _n12.b;
								var g2 = _n13.a;
								var _n14 = _n13.b;
								var b1 = _n14.a;
								var _n15 = _n14.b;
								var b2 = _n15.a;
								var _n16 = _n15.b;
								var a1 = _n16.a;
								var _n17 = _n16.b;
								var a2 = _n17.a;
								return A5(
									ThinkAlexandria$css_in_elm$Css$validHex,
									str,
									_Utils_Tuple2(r1, r2),
									_Utils_Tuple2(g1, g2),
									_Utils_Tuple2(b1, b2),
									_Utils_Tuple2(a1, a2));
							} else {
								break _n0$4;
							}
						}
					} else {
						break _n0$4;
					}
				}
			}
		} else {
			break _n0$4;
		}
	}
	return ThinkAlexandria$css_in_elm$Css$erroneousHex(str);
};
var author$project$Comments$Css$defaultStyleConfig = {
	borderColor: ThinkAlexandria$css_in_elm$Css$hex('#bbb'),
	buttonHoverColor: ThinkAlexandria$css_in_elm$Css$hex('#eee'),
	commonBorderThickness: 3,
	commonRadius: 7,
	editorBackground: ThinkAlexandria$css_in_elm$Css$hex('#fff')
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$AppendProperty = function (a) {
	return {$: 'AppendProperty', a: a};
};
var ThinkAlexandria$css_in_elm$Css$propertyWithWarnings = F3(
	function (warnings, key, value) {
		return ThinkAlexandria$css_in_elm$Css$Preprocess$AppendProperty(
			{important: false, key: key, value: value, warnings: warnings});
	});
var ThinkAlexandria$css_in_elm$Css$property = ThinkAlexandria$css_in_elm$Css$propertyWithWarnings(_List_Nil);
var ThinkAlexandria$css_in_elm$Css$Preprocess$ApplyStyles = function (a) {
	return {$: 'ApplyStyles', a: a};
};
var ThinkAlexandria$css_in_elm$Css$String$fromListString = function (list) {
	return '[ ' + (A2(elm$core$String$join, ', ', list) + ' ]');
};
var ThinkAlexandria$css_in_elm$Css$String$fromMediaExpression = function (mediaExpression) {
	var value = function () {
		var _n0 = mediaExpression.value;
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return 'Just ' + x;
		} else {
			return 'Nothing';
		}
	}();
	return '{ feature = ' + (mediaExpression.feature + (', value = ' + (value + ' }')));
};
var ThinkAlexandria$css_in_elm$Css$String$fromMediaType = function (mediaType) {
	switch (mediaType.$) {
		case 'Print':
			return 'Print';
		case 'Screen':
			return 'Screen';
		default:
			return 'Speech';
	}
};
var ThinkAlexandria$css_in_elm$Css$String$fromMediaQuery = function (mediaQuery) {
	switch (mediaQuery.$) {
		case 'AllQuery':
			var mediaExpressions = mediaQuery.a;
			return 'AllQuery ' + ThinkAlexandria$css_in_elm$Css$String$fromListString(
				A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$String$fromMediaExpression, mediaExpressions));
		case 'OnlyQuery':
			var mediaType = mediaQuery.a;
			var mediaExpressions = mediaQuery.b;
			return 'OnlyQuery ' + (ThinkAlexandria$css_in_elm$Css$String$fromMediaType(mediaType) + (' ' + ThinkAlexandria$css_in_elm$Css$String$fromListString(
				A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$String$fromMediaExpression, mediaExpressions))));
		case 'NotQuery':
			var mediaType = mediaQuery.a;
			var mediaExpressions = mediaQuery.b;
			return 'NotQuery ' + (ThinkAlexandria$css_in_elm$Css$String$fromMediaType(mediaType) + (' ' + ThinkAlexandria$css_in_elm$Css$String$fromListString(
				A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$String$fromMediaExpression, mediaExpressions))));
		default:
			var str = mediaQuery.a;
			return 'CustomQuery ' + str;
	}
};
var ThinkAlexandria$css_in_elm$Css$String$fromListMediaQuery = function (mediaQueries) {
	return ThinkAlexandria$css_in_elm$Css$String$fromListString(
		A2(elm$core$List$map, ThinkAlexandria$css_in_elm$Css$String$fromMediaQuery, mediaQueries));
};
var ThinkAlexandria$css_in_elm$Css$String$fromPseudoElement = function (pseudoElement) {
	var str = pseudoElement.a;
	return 'PseudoElement ' + str;
};
var ThinkAlexandria$css_in_elm$Css$String$fromRepeatableSimpleSelector = function (selector) {
	switch (selector.$) {
		case 'ClassSelector':
			var str = selector.a;
			return 'ClassSelector ' + str;
		case 'IdSelector':
			var str = selector.a;
			return 'IdSelector ' + str;
		default:
			var str = selector.a;
			return 'PseudoClassSelector' + str;
	}
};
var ThinkAlexandria$css_in_elm$Css$String$fromSelectorCombinator = function (selector) {
	switch (selector.$) {
		case 'AdjacentSibling':
			return 'AdjacentSibling';
		case 'GeneralSibling':
			return 'GeneralSibling';
		case 'Child':
			return 'Child';
		default:
			return 'Descendant';
	}
};
var ThinkAlexandria$css_in_elm$Css$getOverloadedProperty = F3(
	function (functionName, desiredKey, style) {
		getOverloadedProperty:
		while (true) {
			switch (style.$) {
				case 'AppendProperty':
					var key = style.a.key;
					return A2(ThinkAlexandria$css_in_elm$Css$property, desiredKey, key);
				case 'ExtendSelector':
					var selector1 = style.a;
					return A3(
						ThinkAlexandria$css_in_elm$Css$propertyWithWarnings,
						_List_fromArray(
							[
								'Cannot apply ' + (functionName + (' with inapplicable Style for selector ' + ThinkAlexandria$css_in_elm$Css$String$fromRepeatableSimpleSelector(selector1)))
							]),
						desiredKey,
						'');
				case 'NestSnippet':
					var combinator = style.a;
					return A3(
						ThinkAlexandria$css_in_elm$Css$propertyWithWarnings,
						_List_fromArray(
							[
								'Cannot apply ' + (functionName + (' with inapplicable Style for combinator ' + ThinkAlexandria$css_in_elm$Css$String$fromSelectorCombinator(combinator)))
							]),
						desiredKey,
						'');
				case 'WithPseudoElement':
					var pseudoElem = style.a;
					return A3(
						ThinkAlexandria$css_in_elm$Css$propertyWithWarnings,
						_List_fromArray(
							[
								'Cannot apply ' + (functionName + (' with inapplicable Style for pseudo-element setter ' + ThinkAlexandria$css_in_elm$Css$String$fromPseudoElement(pseudoElem)))
							]),
						desiredKey,
						'');
				case 'WithMedia':
					var mediaQuery = style.a;
					return A3(
						ThinkAlexandria$css_in_elm$Css$propertyWithWarnings,
						_List_fromArray(
							[
								'Cannot apply ' + (functionName + (' with inapplicable Style for media query ' + ThinkAlexandria$css_in_elm$Css$String$fromListMediaQuery(mediaQuery)))
							]),
						desiredKey,
						'');
				default:
					if (!style.a.b) {
						return A3(
							ThinkAlexandria$css_in_elm$Css$propertyWithWarnings,
							_List_fromArray(
								['Cannot apply ' + (functionName + ' with empty Style. ')]),
							desiredKey,
							'');
					} else {
						if (!style.a.b.b) {
							var _n1 = style.a;
							var only = _n1.a;
							var $temp$functionName = functionName,
								$temp$desiredKey = desiredKey,
								$temp$style = only;
							functionName = $temp$functionName;
							desiredKey = $temp$desiredKey;
							style = $temp$style;
							continue getOverloadedProperty;
						} else {
							var _n2 = style.a;
							var rest = _n2.b;
							var $temp$functionName = functionName,
								$temp$desiredKey = desiredKey,
								$temp$style = ThinkAlexandria$css_in_elm$Css$Preprocess$ApplyStyles(rest);
							functionName = $temp$functionName;
							desiredKey = $temp$desiredKey;
							style = $temp$style;
							continue getOverloadedProperty;
						}
					}
			}
		}
	});
var ThinkAlexandria$css_in_elm$Css$IncompatibleUnits = {$: 'IncompatibleUnits'};
var elm$core$String$fromFloat = _String_fromNumber;
var ThinkAlexandria$css_in_elm$Css$lengthConverter = F3(
	function (units, unitLabel, numericValue) {
		return {
			absoluteLength: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			calc: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			flexBasis: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			fontSize: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			length: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			lengthOrAuto: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			lengthOrAutoOrCoverOrContain: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			lengthOrMinMaxDimension: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			lengthOrNone: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			lengthOrNoneOrMinMaxDimension: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			lengthOrNumber: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			lengthOrNumberOrAutoOrNoneOrContent: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			numericValue: numericValue,
			textIndent: ThinkAlexandria$css_in_elm$Css$Structure$Compatible,
			unitLabel: unitLabel,
			units: units,
			value: _Utils_ap(
				elm$core$String$fromFloat(numericValue),
				unitLabel)
		};
	});
var ThinkAlexandria$css_in_elm$Css$lengthForOverloadedProperty = A3(ThinkAlexandria$css_in_elm$Css$lengthConverter, ThinkAlexandria$css_in_elm$Css$IncompatibleUnits, '', 0);
var ThinkAlexandria$css_in_elm$Css$alignItems = function (fn) {
	return A3(
		ThinkAlexandria$css_in_elm$Css$getOverloadedProperty,
		'alignItems',
		'align-items',
		fn(ThinkAlexandria$css_in_elm$Css$lengthForOverloadedProperty));
};
var ThinkAlexandria$css_in_elm$Css$backgroundColor = function (c) {
	return A3(ThinkAlexandria$css_in_elm$Css$propertyWithWarnings, c.warnings, 'background-color', c.value);
};
var ThinkAlexandria$css_in_elm$Css$prop1 = F2(
	function (key, arg) {
		return A2(ThinkAlexandria$css_in_elm$Css$property, key, arg.value);
	});
var ThinkAlexandria$css_in_elm$Css$border = ThinkAlexandria$css_in_elm$Css$prop1('border');
var ThinkAlexandria$css_in_elm$Css$prop3 = F4(
	function (key, argA, argB, argC) {
		return A2(
			ThinkAlexandria$css_in_elm$Css$property,
			key,
			A2(
				elm$core$String$join,
				' ',
				_List_fromArray(
					[argA.value, argB.value, argC.value])));
	});
var ThinkAlexandria$css_in_elm$Css$border3 = ThinkAlexandria$css_in_elm$Css$prop3('border');
var ThinkAlexandria$css_in_elm$Css$borderBottom = ThinkAlexandria$css_in_elm$Css$prop1('border-bottom');
var ThinkAlexandria$css_in_elm$Css$borderBottom3 = ThinkAlexandria$css_in_elm$Css$prop3('border-bottom');
var ThinkAlexandria$css_in_elm$Css$borderBox = {backgroundClip: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, boxSizing: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, value: 'border-box'};
var ThinkAlexandria$css_in_elm$Css$borderColor = function (c) {
	return A3(ThinkAlexandria$css_in_elm$Css$propertyWithWarnings, c.warnings, 'border-color', c.value);
};
var ThinkAlexandria$css_in_elm$Css$borderLeft3 = ThinkAlexandria$css_in_elm$Css$prop3('border-left');
var ThinkAlexandria$css_in_elm$Css$borderRadius = ThinkAlexandria$css_in_elm$Css$prop1('border-radius');
var ThinkAlexandria$css_in_elm$Css$borderTop3 = ThinkAlexandria$css_in_elm$Css$prop3('border-top');
var ThinkAlexandria$css_in_elm$Css$boxSizing = ThinkAlexandria$css_in_elm$Css$prop1('box-sizing');
var ThinkAlexandria$css_in_elm$Css$center = ThinkAlexandria$css_in_elm$Css$prop1('center');
var ThinkAlexandria$css_in_elm$Css$Preprocess$NestSnippet = F2(
	function (a, b) {
		return {$: 'NestSnippet', a: a, b: b};
	});
var ThinkAlexandria$css_in_elm$Css$Structure$Child = {$: 'Child'};
var ThinkAlexandria$css_in_elm$Css$children = ThinkAlexandria$css_in_elm$Css$Preprocess$NestSnippet(ThinkAlexandria$css_in_elm$Css$Structure$Child);
var ThinkAlexandria$css_in_elm$Css$Preprocess$Snippet = function (a) {
	return {$: 'Snippet', a: a};
};
var ThinkAlexandria$css_in_elm$Css$Preprocess$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var ThinkAlexandria$css_in_elm$Css$Preprocess$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var ThinkAlexandria$css_in_elm$Css$makeSnippet = F2(
	function (styles, sequence) {
		var emptySelector = A3(ThinkAlexandria$css_in_elm$Css$Structure$Selector, sequence, _List_Nil, elm$core$Maybe$Nothing);
		return ThinkAlexandria$css_in_elm$Css$Preprocess$Snippet(
			_List_fromArray(
				[
					ThinkAlexandria$css_in_elm$Css$Preprocess$StyleBlockDeclaration(
					A3(ThinkAlexandria$css_in_elm$Css$Preprocess$StyleBlock, emptySelector, _List_Nil, styles))
				]));
	});
var elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var elm$regex$Regex$fromString = function (string) {
	return A2(
		elm$regex$Regex$fromStringWith,
		{caseInsensitive: false, multiline: false},
		string);
};
var elm$regex$Regex$never = _Regex_never;
var ThinkAlexandria$css_in_elm$Css$Helpers$regex = function (string) {
	return A2(
		elm$core$Maybe$withDefault,
		elm$regex$Regex$never,
		elm$regex$Regex$fromString(string));
};
var elm$core$String$trim = _String_trim;
var elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var ThinkAlexandria$css_in_elm$Css$Helpers$toCssIdentifier = function (identifier) {
	return A3(
		elm$regex$Regex$replace,
		ThinkAlexandria$css_in_elm$Css$Helpers$regex('[^a-zA-Z0-9_-]'),
		function (_n1) {
			return '';
		},
		A3(
			elm$regex$Regex$replace,
			ThinkAlexandria$css_in_elm$Css$Helpers$regex('\\s+'),
			function (_n0) {
				return '-';
			},
			elm$core$String$trim(identifier)));
};
var ThinkAlexandria$css_in_elm$Css$Helpers$identifierToString = F2(
	function (name, identifier) {
		return _Utils_ap(
			ThinkAlexandria$css_in_elm$Css$Helpers$toCssIdentifier(name),
			ThinkAlexandria$css_in_elm$Css$Helpers$toCssIdentifier(identifier));
	});
var ThinkAlexandria$css_in_elm$Css$Structure$ClassSelector = function (a) {
	return {$: 'ClassSelector', a: a};
};
var ThinkAlexandria$css_in_elm$Css$class = F2(
	function (className, styles) {
		return A2(
			ThinkAlexandria$css_in_elm$Css$makeSnippet,
			styles,
			ThinkAlexandria$css_in_elm$Css$Structure$UniversalSelectorSequence(
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$Structure$ClassSelector(
						A2(ThinkAlexandria$css_in_elm$Css$Helpers$identifierToString, '', className))
					])));
	});
var ThinkAlexandria$css_in_elm$Css$color = function (c) {
	return A3(ThinkAlexandria$css_in_elm$Css$propertyWithWarnings, c.warnings, 'color', c.value);
};
var ThinkAlexandria$css_in_elm$Css$displayFlex = A2(ThinkAlexandria$css_in_elm$Css$property, 'display', 'flex');
var ThinkAlexandria$css_in_elm$Css$height = ThinkAlexandria$css_in_elm$Css$prop1('height');
var ThinkAlexandria$css_in_elm$Css$Preprocess$ExtendSelector = F2(
	function (a, b) {
		return {$: 'ExtendSelector', a: a, b: b};
	});
var ThinkAlexandria$css_in_elm$Css$Structure$PseudoClassSelector = function (a) {
	return {$: 'PseudoClassSelector', a: a};
};
var ThinkAlexandria$css_in_elm$Css$pseudoClass = function (className) {
	return ThinkAlexandria$css_in_elm$Css$Preprocess$ExtendSelector(
		ThinkAlexandria$css_in_elm$Css$Structure$PseudoClassSelector(className));
};
var ThinkAlexandria$css_in_elm$Css$hover = ThinkAlexandria$css_in_elm$Css$pseudoClass('hover');
var ThinkAlexandria$css_in_elm$Css$justifyContent = function (fn) {
	return A3(
		ThinkAlexandria$css_in_elm$Css$getOverloadedProperty,
		'justifyContent',
		'justify-content',
		fn(ThinkAlexandria$css_in_elm$Css$lengthForOverloadedProperty));
};
var ThinkAlexandria$css_in_elm$Css$margin = ThinkAlexandria$css_in_elm$Css$prop1('margin');
var ThinkAlexandria$css_in_elm$Css$prop2 = F3(
	function (key, argA, argB) {
		return A2(
			ThinkAlexandria$css_in_elm$Css$property,
			key,
			A2(
				elm$core$String$join,
				' ',
				_List_fromArray(
					[argA.value, argB.value])));
	});
var ThinkAlexandria$css_in_elm$Css$margin2 = ThinkAlexandria$css_in_elm$Css$prop2('margin');
var ThinkAlexandria$css_in_elm$Css$marginBottom = ThinkAlexandria$css_in_elm$Css$prop1('margin-bottom');
var ThinkAlexandria$css_in_elm$Css$marginLeft = ThinkAlexandria$css_in_elm$Css$prop1('margin-left');
var ThinkAlexandria$css_in_elm$Css$marginTop = ThinkAlexandria$css_in_elm$Css$prop1('margin-top');
var ThinkAlexandria$css_in_elm$Css$maxHeight = ThinkAlexandria$css_in_elm$Css$prop1('max-height');
var ThinkAlexandria$css_in_elm$Css$middle = ThinkAlexandria$css_in_elm$Css$prop1('middle');
var ThinkAlexandria$css_in_elm$Css$minHeight = ThinkAlexandria$css_in_elm$Css$prop1('min-height');
var ThinkAlexandria$css_in_elm$Css$padding = ThinkAlexandria$css_in_elm$Css$prop1('padding');
var ThinkAlexandria$css_in_elm$Css$padding2 = ThinkAlexandria$css_in_elm$Css$prop2('padding');
var ThinkAlexandria$css_in_elm$Css$padding3 = ThinkAlexandria$css_in_elm$Css$prop3('padding');
var ThinkAlexandria$css_in_elm$Css$PercentageUnits = {$: 'PercentageUnits'};
var ThinkAlexandria$css_in_elm$Css$pct = A2(ThinkAlexandria$css_in_elm$Css$lengthConverter, ThinkAlexandria$css_in_elm$Css$PercentageUnits, '%');
var ThinkAlexandria$css_in_elm$Css$PxUnits = {$: 'PxUnits'};
var ThinkAlexandria$css_in_elm$Css$px = A2(ThinkAlexandria$css_in_elm$Css$lengthConverter, ThinkAlexandria$css_in_elm$Css$PxUnits, 'px');
var ThinkAlexandria$css_in_elm$Css$resize = ThinkAlexandria$css_in_elm$Css$prop1('resize');
var ThinkAlexandria$css_in_elm$Css$selector = F2(
	function (selectorStr, styles) {
		return A2(
			ThinkAlexandria$css_in_elm$Css$makeSnippet,
			styles,
			A2(ThinkAlexandria$css_in_elm$Css$Structure$CustomSelector, selectorStr, _List_Nil));
	});
var ThinkAlexandria$css_in_elm$Css$solid = {borderStyle: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, textDecorationStyle: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, value: 'solid'};
var ThinkAlexandria$css_in_elm$Css$spaceBetween = ThinkAlexandria$css_in_elm$Css$prop1('space-between');
var ThinkAlexandria$css_in_elm$Css$vertical = {resize: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, value: 'vertical'};
var ThinkAlexandria$css_in_elm$Css$verticalAlign = function (fn) {
	return A3(
		ThinkAlexandria$css_in_elm$Css$getOverloadedProperty,
		'verticalAlign',
		'vertical-align',
		fn(ThinkAlexandria$css_in_elm$Css$lengthForOverloadedProperty));
};
var ThinkAlexandria$css_in_elm$Css$width = ThinkAlexandria$css_in_elm$Css$prop1('width');
var ThinkAlexandria$css_in_elm$Css$Structure$TypeSelector = function (a) {
	return {$: 'TypeSelector', a: a};
};
var ThinkAlexandria$css_in_elm$Css$Elements$typeSelector = F2(
	function (selectorStr, styles) {
		var sequence = A2(
			ThinkAlexandria$css_in_elm$Css$Structure$TypeSelectorSequence,
			ThinkAlexandria$css_in_elm$Css$Structure$TypeSelector(selectorStr),
			_List_Nil);
		var selector = A3(ThinkAlexandria$css_in_elm$Css$Structure$Selector, sequence, _List_Nil, elm$core$Maybe$Nothing);
		return ThinkAlexandria$css_in_elm$Css$Preprocess$Snippet(
			_List_fromArray(
				[
					ThinkAlexandria$css_in_elm$Css$Preprocess$StyleBlockDeclaration(
					A3(ThinkAlexandria$css_in_elm$Css$Preprocess$StyleBlock, selector, _List_Nil, styles))
				]));
	});
var ThinkAlexandria$css_in_elm$Css$Elements$pre = ThinkAlexandria$css_in_elm$Css$Elements$typeSelector('pre');
var author$project$Comments$Css$styleCommentEditor = F2(
	function (styleConfig, cssConfig) {
		var _class = A2(elm$core$Basics$composeR, cssConfig.toString, ThinkAlexandria$css_in_elm$Css$class);
		return _List_fromArray(
			[
				A2(
				_class,
				cssConfig.editor,
				_List_fromArray(
					[
						A3(
						ThinkAlexandria$css_in_elm$Css$border3,
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonBorderThickness),
						ThinkAlexandria$css_in_elm$Css$solid,
						styleConfig.borderColor),
						ThinkAlexandria$css_in_elm$Css$backgroundColor(styleConfig.editorBackground),
						ThinkAlexandria$css_in_elm$Css$margin(
						ThinkAlexandria$css_in_elm$Css$px(0)),
						ThinkAlexandria$css_in_elm$Css$borderRadius(
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonRadius))
					])),
				A2(
				_class,
				cssConfig.button,
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$marginLeft(
						ThinkAlexandria$css_in_elm$Css$px(5)),
						ThinkAlexandria$css_in_elm$Css$backgroundColor(styleConfig.editorBackground),
						A3(
						ThinkAlexandria$css_in_elm$Css$border3,
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonBorderThickness),
						ThinkAlexandria$css_in_elm$Css$solid,
						styleConfig.borderColor),
						A2(
						ThinkAlexandria$css_in_elm$Css$padding2,
						ThinkAlexandria$css_in_elm$Css$px(8),
						ThinkAlexandria$css_in_elm$Css$px(12)),
						ThinkAlexandria$css_in_elm$Css$borderRadius(
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonRadius)),
						ThinkAlexandria$css_in_elm$Css$hover(
						_List_fromArray(
							[
								ThinkAlexandria$css_in_elm$Css$backgroundColor(styleConfig.buttonHoverColor)
							]))
					])),
				A2(
				_class,
				cssConfig.header,
				_List_fromArray(
					[
						A3(
						ThinkAlexandria$css_in_elm$Css$padding3,
						ThinkAlexandria$css_in_elm$Css$px(6),
						ThinkAlexandria$css_in_elm$Css$px(10),
						ThinkAlexandria$css_in_elm$Css$px(0)),
						ThinkAlexandria$css_in_elm$Css$displayFlex,
						ThinkAlexandria$css_in_elm$Css$justifyContent(ThinkAlexandria$css_in_elm$Css$spaceBetween),
						ThinkAlexandria$css_in_elm$Css$alignItems(ThinkAlexandria$css_in_elm$Css$center)
					])),
				A2(
				_class,
				cssConfig.horizontalTabSelected,
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$borderColor(styleConfig.borderColor),
						ThinkAlexandria$css_in_elm$Css$backgroundColor(styleConfig.editorBackground),
						ThinkAlexandria$css_in_elm$Css$borderBottom(
						ThinkAlexandria$css_in_elm$Css$px(0))
					])),
				A2(
				_class,
				cssConfig.toolbarButton,
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$margin(
						ThinkAlexandria$css_in_elm$Css$px(5)),
						ThinkAlexandria$css_in_elm$Css$padding(
						ThinkAlexandria$css_in_elm$Css$px(0)),
						ThinkAlexandria$css_in_elm$Css$border(
						ThinkAlexandria$css_in_elm$Css$px(0)),
						ThinkAlexandria$css_in_elm$Css$backgroundColor(
						ThinkAlexandria$css_in_elm$Css$hex('#fff'))
					])),
				A2(
				_class,
				cssConfig.body,
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$margin(
						ThinkAlexandria$css_in_elm$Css$px(10)),
						ThinkAlexandria$css_in_elm$Css$marginTop(
						ThinkAlexandria$css_in_elm$Css$px(0)),
						A3(
						ThinkAlexandria$css_in_elm$Css$borderTop3,
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonBorderThickness),
						ThinkAlexandria$css_in_elm$Css$solid,
						styleConfig.borderColor),
						A3(
						ThinkAlexandria$css_in_elm$Css$borderBottom3,
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonBorderThickness),
						ThinkAlexandria$css_in_elm$Css$solid,
						styleConfig.borderColor)
					])),
				A2(
				_class,
				cssConfig.markdownBody,
				_List_fromArray(
					[
						A3(
						ThinkAlexandria$css_in_elm$Css$borderTop3,
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonBorderThickness),
						ThinkAlexandria$css_in_elm$Css$solid,
						styleConfig.borderColor),
						A2(
						ThinkAlexandria$css_in_elm$Css$margin2,
						ThinkAlexandria$css_in_elm$Css$px(0),
						ThinkAlexandria$css_in_elm$Css$px(10)),
						A2(
						ThinkAlexandria$css_in_elm$Css$padding2,
						ThinkAlexandria$css_in_elm$Css$px(10),
						ThinkAlexandria$css_in_elm$Css$px(5)),
						ThinkAlexandria$css_in_elm$Css$children(
						_List_fromArray(
							[
								A2(
								ThinkAlexandria$css_in_elm$Css$selector,
								':last-child',
								_List_fromArray(
									[
										ThinkAlexandria$css_in_elm$Css$marginBottom(
										ThinkAlexandria$css_in_elm$Css$px(0))
									])),
								A2(
								ThinkAlexandria$css_in_elm$Css$selector,
								':first-child',
								_List_fromArray(
									[
										ThinkAlexandria$css_in_elm$Css$marginTop(
										ThinkAlexandria$css_in_elm$Css$px(0))
									])),
								ThinkAlexandria$css_in_elm$Css$Elements$pre(
								_List_fromArray(
									[
										ThinkAlexandria$css_in_elm$Css$backgroundColor(
										ThinkAlexandria$css_in_elm$Css$hex('#272822')),
										ThinkAlexandria$css_in_elm$Css$color(
										ThinkAlexandria$css_in_elm$Css$hex('#ddd')),
										ThinkAlexandria$css_in_elm$Css$padding(
										ThinkAlexandria$css_in_elm$Css$px(10)),
										ThinkAlexandria$css_in_elm$Css$borderRadius(
										ThinkAlexandria$css_in_elm$Css$px(5))
									]))
							]))
					])),
				A2(
				_class,
				cssConfig.textInput,
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$width(
						ThinkAlexandria$css_in_elm$Css$pct(100)),
						ThinkAlexandria$css_in_elm$Css$padding(
						ThinkAlexandria$css_in_elm$Css$px(10)),
						ThinkAlexandria$css_in_elm$Css$minHeight(
						ThinkAlexandria$css_in_elm$Css$px(100)),
						ThinkAlexandria$css_in_elm$Css$maxHeight(
						ThinkAlexandria$css_in_elm$Css$px(500)),
						ThinkAlexandria$css_in_elm$Css$border(
						ThinkAlexandria$css_in_elm$Css$px(0)),
						ThinkAlexandria$css_in_elm$Css$resize(ThinkAlexandria$css_in_elm$Css$vertical),
						ThinkAlexandria$css_in_elm$Css$boxSizing(ThinkAlexandria$css_in_elm$Css$borderBox)
					])),
				A2(
				_class,
				cssConfig.footer,
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$displayFlex,
						ThinkAlexandria$css_in_elm$Css$justifyContent(ThinkAlexandria$css_in_elm$Css$spaceBetween),
						ThinkAlexandria$css_in_elm$Css$margin(
						ThinkAlexandria$css_in_elm$Css$px(10))
					])),
				A2(
				ThinkAlexandria$css_in_elm$Css$class,
				'octiconMarkdown',
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$verticalAlign(ThinkAlexandria$css_in_elm$Css$middle)
					])),
				A2(
				_class,
				cssConfig.spacer,
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$height(
						ThinkAlexandria$css_in_elm$Css$px(30)),
						ThinkAlexandria$css_in_elm$Css$marginLeft(
						ThinkAlexandria$css_in_elm$Css$px(30)),
						A3(
						ThinkAlexandria$css_in_elm$Css$borderLeft3,
						ThinkAlexandria$css_in_elm$Css$px(3),
						ThinkAlexandria$css_in_elm$Css$solid,
						styleConfig.borderColor)
					]))
			]);
	});
var ThinkAlexandria$css_in_elm$Css$borderTopLeftRadius = ThinkAlexandria$css_in_elm$Css$prop1('border-top-left-radius');
var ThinkAlexandria$css_in_elm$Css$borderTopRightRadius = ThinkAlexandria$css_in_elm$Css$prop1('border-top-right-radius');
var ThinkAlexandria$css_in_elm$Css$EmUnits = {$: 'EmUnits'};
var ThinkAlexandria$css_in_elm$Css$em = A2(ThinkAlexandria$css_in_elm$Css$lengthConverter, ThinkAlexandria$css_in_elm$Css$EmUnits, 'em');
var ThinkAlexandria$css_in_elm$Css$initial = {alignItems: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, all: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, backgroundAttachment: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, backgroundBlendMode: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, backgroundImage: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, backgroundOrigin: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, backgroundRepeat: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, backgroundRepeatShorthand: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, borderStyle: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, boxSizing: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, color: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, cursor: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, display: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, flexBasis: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, flexDirection: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, flexDirectionOrWrap: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, flexWrap: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, fontFamily: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, fontSize: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, fontStyle: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, fontVariant: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, fontWeight: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, intOrAuto: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, justifyContent: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, length: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, lengthOrAuto: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, lengthOrAutoOrCoverOrContain: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, lengthOrMinMaxDimension: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, lengthOrNone: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, lengthOrNoneOrMinMaxDimension: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, lengthOrNumber: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, lengthOrNumberOrAutoOrNoneOrContent: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, listStylePosition: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, listStyleType: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, listStyleTypeOrPositionOrImage: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, none: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, number: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, numericValue: 0, outline: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, overflow: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, textDecorationLine: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, textDecorationStyle: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, textIndent: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, textRendering: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, textTransform: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, touchAction: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, unitLabel: '', units: ThinkAlexandria$css_in_elm$Css$IncompatibleUnits, value: 'initial', warnings: _List_Nil, whiteSpace: ThinkAlexandria$css_in_elm$Css$Structure$Compatible};
var ThinkAlexandria$css_in_elm$Css$inherit = _Utils_update(
	ThinkAlexandria$css_in_elm$Css$initial,
	{value: 'inherit'});
var ThinkAlexandria$css_in_elm$Css$minWidth = ThinkAlexandria$css_in_elm$Css$prop1('min-width');
var ThinkAlexandria$css_in_elm$Css$transparent = {color: ThinkAlexandria$css_in_elm$Css$Structure$Compatible, value: 'transparent', warnings: _List_Nil};
var author$project$Comments$Css$styleHorizontalTabNav = F2(
	function (styleConfig, cssConfig) {
		var _class = A2(elm$core$Basics$composeR, cssConfig.toString, ThinkAlexandria$css_in_elm$Css$class);
		return _List_fromArray(
			[
				A2(
				_class,
				cssConfig.horizontalTabNav,
				_List_fromArray(
					[
						ThinkAlexandria$css_in_elm$Css$marginBottom(
						ThinkAlexandria$css_in_elm$Css$px(2 * (-styleConfig.commonBorderThickness))),
						ThinkAlexandria$css_in_elm$Css$borderColor(ThinkAlexandria$css_in_elm$Css$inherit)
					])),
				A2(
				_class,
				cssConfig.horizontalTab,
				_List_fromArray(
					[
						A3(
						ThinkAlexandria$css_in_elm$Css$border3,
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonBorderThickness),
						ThinkAlexandria$css_in_elm$Css$solid,
						ThinkAlexandria$css_in_elm$Css$transparent),
						ThinkAlexandria$css_in_elm$Css$backgroundColor(ThinkAlexandria$css_in_elm$Css$transparent),
						A2(
						ThinkAlexandria$css_in_elm$Css$padding2,
						ThinkAlexandria$css_in_elm$Css$px(8),
						ThinkAlexandria$css_in_elm$Css$px(12)),
						ThinkAlexandria$css_in_elm$Css$borderTopLeftRadius(
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonRadius)),
						ThinkAlexandria$css_in_elm$Css$borderTopRightRadius(
						ThinkAlexandria$css_in_elm$Css$px(styleConfig.commonRadius)),
						ThinkAlexandria$css_in_elm$Css$minWidth(
						ThinkAlexandria$css_in_elm$Css$em(4)),
						ThinkAlexandria$css_in_elm$Css$color(ThinkAlexandria$css_in_elm$Css$inherit)
					]))
			]);
	});
var author$project$Comments$Css$exampleStyleSheet = ThinkAlexandria$css_in_elm$Css$stylesheet(
	elm$core$List$concat(
		_List_fromArray(
			[
				A2(author$project$Comments$Css$styleHorizontalTabNav, author$project$Comments$Css$defaultStyleConfig, author$project$Comments$Css$defaultCssConfig),
				A2(author$project$Comments$Css$styleCommentEditor, author$project$Comments$Css$defaultStyleConfig, author$project$Comments$Css$defaultCssConfig)
			])));
var author$project$Example$NewPost = {$: 'NewPost'};
var author$project$Comments$DeleteCommentDraft = function (a) {
	return {$: 'DeleteCommentDraft', a: a};
};
var author$project$Comments$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var author$project$Comments$New = function (a) {
	return {$: 'New', a: a};
};
var author$project$Comments$NewComment = function (a) {
	return {$: 'NewComment', a: a};
};
var author$project$Comments$CreateCommentDraft = function (a) {
	return {$: 'CreateCommentDraft', a: a};
};
var author$project$Comments$PreviewCommentDraft = F2(
	function (a, b) {
		return {$: 'PreviewCommentDraft', a: a, b: b};
	});
var author$project$Comments$UpdateCommentDraft = F2(
	function (a, b) {
		return {$: 'UpdateCommentDraft', a: a, b: b};
	});
var ChristophP$elm_i18next$I18Next$t = F2(
	function (_n0, key) {
		var translations = _n0.a;
		return A2(
			elm$core$Maybe$withDefault,
			key,
			A2(elm$core$Dict$get, key, translations));
	});
var author$project$Comments$translate = ChristophP$elm_i18next$I18Next$t;
var ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$toString = function (a) {
	switch (a.$) {
		case 'ToolTipped':
			return 'ToolTipped';
		case 'ToolTippedMultiline':
			return 'ToolTippedMultiline';
		case 'ToolTippedNoDelay':
			return 'ToolTippedNoDelay';
		case 'ToolTippedSticky':
			return 'ToolTippedSticky';
		case 'ToolTippedN':
			return 'ToolTippedN';
		case 'ToolTippedS':
			return 'ToolTippedS';
		case 'ToolTippedE':
			return 'ToolTippedE';
		case 'ToolTippedW':
			return 'ToolTippedW';
		case 'ToolTippedNE':
			return 'ToolTippedNE';
		case 'ToolTippedNW':
			return 'ToolTippedNW';
		case 'ToolTippedSE':
			return 'ToolTippedSE';
		default:
			return 'ToolTippedSW';
	}
};
var elm$json$Json$Encode$string = _Json_wrap;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$html$Html$Attributes$classList = function (classes) {
	return elm$html$Html$Attributes$class(
		A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$map,
				elm$core$Tuple$first,
				A2(elm$core$List$filter, elm$core$Tuple$second, classes))));
};
var ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList = function (names) {
	return elm$html$Html$Attributes$classList(
		A2(
			elm$core$List$map,
			function (_n0) {
				var c = _n0.a;
				var b = _n0.b;
				return _Utils_Tuple2(
					ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$toString(c),
					b);
			},
			names));
};
var ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped = {$: 'ToolTipped'};
var ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedS = {$: 'ToolTippedS'};
var ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedSE = {$: 'ToolTippedSE'};
var capitalist$elm_octicons$Octicons$defaultOptions = {_class: elm$core$Maybe$Nothing, color: 'black', fillRule: 'evenodd', height: 16, margin: elm$core$Maybe$Nothing, style: elm$core$Maybe$Nothing, width: 16};
var author$project$Comments$octiconsDefaultOptions = capitalist$elm_octicons$Octicons$defaultOptions;
var capitalist$elm_octicons$Octicons$boldPath = 'M1,2 L4.83,2 C7.31,2 9.13,2.75 9.13,4.95 C9.13,6.09 8.5,7.18 7.46,7.56 L7.46,7.62 C8.79,7.92 9.76,8.85 9.76,10.48 C9.76,12.87 7.79,14 5.15,14 L1,14 L1,2 L1,2 Z M4.66,6.95 C6.33,6.95 7.04,6.29 7.04,5.26 C7.04,4.09 6.26,3.65 4.7,3.65 L3.13,3.65 L3.13,6.95 L4.66,6.95 L4.66,6.95 Z M4.93,12.34 C6.7,12.34 7.68,11.7 7.68,10.36 C7.68,9.09 6.73,8.55 4.93,8.55 L3.13,8.55 L3.13,12.35 L4.93,12.35 L4.93,12.34 Z';
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$style = _VirtualDom_attribute('style');
var elm$svg$Svg$Attributes$version = _VirtualDom_attribute('version');
var elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var capitalist$elm_octicons$Octicons$Internal$iconSVG = F5(
	function (viewBox, name, options, attributes, children) {
		var style = function () {
			var _n2 = options.style;
			if (_n2.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var s = _n2.a;
				return _List_fromArray(
					[s]);
			}
		}();
		var margin = function () {
			var _n1 = options.margin;
			if (_n1.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var m = _n1.a;
				return _List_fromArray(
					['margin: ' + m]);
			}
		}();
		var styles = function () {
			var _n0 = elm$core$List$concat(
				_List_fromArray(
					[style, margin]));
			if (!_n0.b) {
				return _List_Nil;
			} else {
				var lst = _n0;
				return _List_fromArray(
					[
						elm$svg$Svg$Attributes$style(
						A2(elm$core$String$join, ';', lst))
					]);
			}
		}();
		return A2(
			elm$svg$Svg$svg,
			elm$core$List$concat(
				_List_fromArray(
					[
						_List_fromArray(
						[
							elm$svg$Svg$Attributes$version('1.1'),
							elm$svg$Svg$Attributes$class(
							A2(elm$core$Maybe$withDefault, 'octicon ' + name, options._class)),
							elm$svg$Svg$Attributes$width(
							elm$core$String$fromInt(options.width)),
							elm$svg$Svg$Attributes$height(
							elm$core$String$fromInt(options.height)),
							elm$svg$Svg$Attributes$viewBox(viewBox)
						]),
						attributes,
						styles
					])),
			children);
	});
var elm$svg$Svg$path = elm$svg$Svg$trustedNode('path');
var elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var elm$svg$Svg$Attributes$fillRule = _VirtualDom_attribute('fill-rule');
var capitalist$elm_octicons$Octicons$pathIconWithOptions = F4(
	function (path, viewBox, octiconName, options) {
		return A5(
			capitalist$elm_octicons$Octicons$Internal$iconSVG,
			viewBox,
			octiconName,
			options,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$svg$Svg$path,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$d(path),
							elm$svg$Svg$Attributes$fillRule(options.fillRule),
							elm$svg$Svg$Attributes$fill(options.color)
						]),
					_List_Nil)
				]));
	});
var capitalist$elm_octicons$Octicons$bold = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$boldPath, '0 0 10 16', 'bold');
var capitalist$elm_octicons$Octicons$codePath = 'M9.5,3 L8,4.5 L11.5,8 L8,11.5 L9.5,13 L14,8 L9.5,3 L9.5,3 Z M4.5,3 L0,8 L4.5,13 L6,11.5 L2.5,8 L6,4.5 L4.5,3 L4.5,3 Z';
var capitalist$elm_octicons$Octicons$code = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$codePath, '0 0 14 16', 'code');
var capitalist$elm_octicons$Octicons$italicPath = 'M2.81,5 L4.79,5 L3,14 L1,14 L2.81,5 L2.81,5 Z M3.17,2.3 C3.17,1.6 3.75,1 4.5,1 C5.06,1 5.63,1.38 5.63,2.03 C5.63,2.78 5.04,3.33 4.3,3.33 C3.72,3.33 3.17,2.95 3.17,2.3 L3.17,2.3 Z';
var capitalist$elm_octicons$Octicons$italic = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$italicPath, '0 0 6 16', 'italic');
var capitalist$elm_octicons$Octicons$listOrderedPath = 'M12,13 C12,13.59 12,14 11.41,14 L4.59,14 C4,14 4,13.59 4,13 C4,12.41 4,12 4.59,12 L11.4,12 C11.99,12 11.99,12.41 11.99,13 L12,13 Z M4.59,4 L11.4,4 C11.99,4 11.99,3.59 11.99,3 C11.99,2.41 11.99,2 11.4,2 L4.59,2 C4,2 4,2.41 4,3 C4,3.59 4,4 4.59,4 L4.59,4 Z M11.4,7 L4.59,7 C4,7 4,7.41 4,8 C4,8.59 4,9 4.59,9 L11.4,9 C11.99,9 11.99,8.59 11.99,8 C11.99,7.41 11.99,7 11.4,7 L11.4,7 Z M2,1 L1.28,1 C0.98,1.19 0.7,1.25 0.25,1.34 L0.25,2 L1,2 L1,4.14 L0.16,4.14 L0.16,5 L3,5 L3,4.14 L2,4.14 L2,1 L2,1 Z M2.25,9.13 C2.08,9.13 1.8,9.16 1.59,9.19 C2.12,8.63 2.73,7.94 2.73,7.3 C2.71,6.52 2.17,6 1.37,6 C0.78,6 0.4,6.2 -0.01,6.64 L0.57,7.22 C0.76,7.03 0.95,6.84 1.21,6.84 C1.49,6.84 1.69,7 1.69,7.36 C1.69,7.89 0.92,8.56 -0.01,9.42 L-0.01,10 L2.99,10 L2.9,9.12 L2.24,9.12 L2.25,9.13 Z M2.17,12.91 L2.17,12.88 C2.61,12.69 2.81,12.41 2.81,12.02 C2.81,11.32 2.25,10.91 1.37,10.91 C0.89,10.91 0.48,11.1 0.09,11.43 L0.64,12.07 C0.89,11.87 1.08,11.76 1.33,11.76 C1.6,11.76 1.75,11.89 1.75,12.12 C1.75,12.39 1.55,12.56 0.89,12.56 L0.89,13.31 C1.72,13.31 1.87,13.48 1.87,13.78 C1.87,14.03 1.64,14.16 1.29,14.16 C1.01,14.16 0.73,14.02 0.48,13.78 L-2.22044605e-16,14.44 C0.3,14.8 0.77,15 1.41,15 C2.24,15 2.94,14.59 2.94,13.84 C2.94,13.34 2.63,13.03 2.17,12.9 L2.17,12.91 Z';
var capitalist$elm_octicons$Octicons$listOrdered = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$listOrderedPath, '0 0 12 16', 'listOrdered');
var capitalist$elm_octicons$Octicons$listUnorderedPath = 'M2,13 C2,13.59 2,14 1.41,14 L0.59,14 C0,14 0,13.59 0,13 C0,12.41 0,12 0.59,12 L1.4,12 C1.99,12 1.99,12.41 1.99,13 L2,13 Z M4.59,4 L11.4,4 C11.99,4 11.99,3.59 11.99,3 C11.99,2.41 11.99,2 11.4,2 L4.59,2 C4,2 4,2.41 4,3 C4,3.59 4,4 4.59,4 L4.59,4 Z M1.41,7 L0.59,7 C0,7 0,7.41 0,8 C0,8.59 0,9 0.59,9 L1.4,9 C1.99,9 1.99,8.59 1.99,8 C1.99,7.41 1.99,7 1.4,7 L1.41,7 Z M1.41,2 L0.59,2 C0,2 0,2.41 0,3 C0,3.59 0,4 0.59,4 L1.4,4 C1.99,4 1.99,3.59 1.99,3 C1.99,2.41 1.99,2 1.4,2 L1.41,2 Z M11.41,7 L4.59,7 C4,7 4,7.41 4,8 C4,8.59 4,9 4.59,9 L11.4,9 C11.99,9 11.99,8.59 11.99,8 C11.99,7.41 11.99,7 11.4,7 L11.41,7 Z M11.41,12 L4.59,12 C4,12 4,12.41 4,13 C4,13.59 4,14 4.59,14 L11.4,14 C11.99,14 11.99,13.59 11.99,13 C11.99,12.41 11.99,12 11.4,12 L11.41,12 Z';
var capitalist$elm_octicons$Octicons$listUnordered = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$listUnorderedPath, '0 0 12 16', 'listUnordered');
var capitalist$elm_octicons$Octicons$quotePath = 'M6.16,3.50000004 C3.73,5.06000004 2.55,6.67000004 2.55,9.36000004 C2.71,9.31000004 2.85,9.31000004 2.99,9.31000004 C4.26,9.31000004 5.49,10.17 5.49,11.72 C5.49,13.33 4.46,14.33 2.99,14.33 C1.09,14.33 0,12.81 0,10.08 C0,6.28000004 1.75,3.55000004 5.02,1.66000004 L6.16,3.50000004 L6.16,3.50000004 Z M13.16,3.50000004 C10.73,5.06000004 9.55,6.67000004 9.55,9.36000004 C9.71,9.31000004 9.85,9.31000004 9.99,9.31000004 C11.26,9.31000004 12.49,10.17 12.49,11.72 C12.49,13.33 11.46,14.33 9.99,14.33 C8.1,14.33 7.01,12.81 7.01,10.08 C7.01,6.28000004 8.76,3.55000004 12.03,1.66000004 L13.17,3.50000004 L13.16,3.50000004 Z';
var capitalist$elm_octicons$Octicons$quote = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$quotePath, '0 0 14 16', 'quote');
var capitalist$elm_octicons$Octicons$tasklistPath = 'M15.41,9 L7.59,9 C7,9 7,8.59 7,8 C7,7.41 7,7 7.59,7 L15.4,7 C15.99,7 15.99,7.41 15.99,8 C15.99,8.59 15.99,9 15.4,9 L15.41,9 Z M9.59,4 C9,4 9,3.59 9,3 C9,2.41 9,2 9.59,2 L15.4,2 C15.99,2 15.99,2.41 15.99,3 C15.99,3.59 15.99,4 15.4,4 L9.59,4 L9.59,4 Z M0,3.91 L1.41,2.61 L3,4.2 L7.09,0 L8.5,1.41 L3,6.91 L0,3.91 L0,3.91 Z M7.59,12 L15.4,12 C15.99,12 15.99,12.41 15.99,13 C15.99,13.59 15.99,14 15.4,14 L7.59,14 C7,14 7,13.59 7,13 C7,12.41 7,12 7.59,12 L7.59,12 Z';
var capitalist$elm_octicons$Octicons$tasklist = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$tasklistPath, '0 0 16 16', 'tasklist');
var capitalist$elm_octicons$Octicons$textSizePath = 'M13.62,9.08 L12.1,3.66 L12.04,3.66 L10.54,9.08 L13.62,9.08 Z M5.7,10.13 C5.7,10.13 4.68,6.52 4.53,6.02 L4.45,6.02 L3.32,10.13 L5.7,10.13 Z M17.31,14 L15.06,14 L14.11,10.75 L10.04,10.75 L9.09,14 L6.84,14 L6.15,11.67 L2.87,11.67 L2.17,14 L0,14 L3.3,4.41 L5.8,4.41 L7.97,10.75 L10.86,2 L13.38,2 L17.32,14 L17.31,14 Z';
var capitalist$elm_octicons$Octicons$textSize = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$textSizePath, '0 0 18 16', 'textSize');
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$span = _VirtualDom_node('span');
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var author$project$Comments$viewToolbar = F2(
	function (config, showMarkdownPreview) {
		var _class = function (value) {
			return elm$html$Html$Attributes$class(
				config.cssConfig.toString(value));
		};
		return A2(
			elm$html$Html$div,
			_List_Nil,
			showMarkdownPreview ? _List_Nil : _List_fromArray(
				[
					A2(
					elm$html$Html$span,
					_List_fromArray(
						[
							_class(config.cssConfig.toolbarButton),
							ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped, true),
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedSE, true)
								])),
							A2(
							elm$html$Html$Attributes$attribute,
							'aria-label',
							A2(author$project$Comments$translate, config.translations, 'comment-editor-tooltip-text-size'))
						]),
					_List_fromArray(
						[
							capitalist$elm_octicons$Octicons$textSize(
							_Utils_update(
								author$project$Comments$octiconsDefaultOptions,
								{width: 18}))
						])),
					A2(
					elm$html$Html$span,
					_List_fromArray(
						[
							_class(config.cssConfig.toolbarButton),
							ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped, true),
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedSE, true)
								])),
							A2(
							elm$html$Html$Attributes$attribute,
							'aria-label',
							A2(author$project$Comments$translate, config.translations, 'comment-editor-tooltip-bold-text'))
						]),
					_List_fromArray(
						[
							capitalist$elm_octicons$Octicons$bold(
							_Utils_update(
								author$project$Comments$octiconsDefaultOptions,
								{width: 10}))
						])),
					A2(
					elm$html$Html$span,
					_List_fromArray(
						[
							_class(config.cssConfig.toolbarButton),
							ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped, true),
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedS, true)
								])),
							A2(
							elm$html$Html$Attributes$attribute,
							'aria-label',
							A2(author$project$Comments$translate, config.translations, 'comment-editor-tooltip-italic-text'))
						]),
					_List_fromArray(
						[
							capitalist$elm_octicons$Octicons$italic(
							_Utils_update(
								author$project$Comments$octiconsDefaultOptions,
								{width: 6}))
						])),
					A2(
					elm$html$Html$span,
					_List_fromArray(
						[
							_class(config.cssConfig.toolbarButton),
							ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped, true),
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedS, true)
								])),
							A2(
							elm$html$Html$Attributes$attribute,
							'aria-label',
							A2(author$project$Comments$translate, config.translations, 'comment-editor-tooltip-code'))
						]),
					_List_fromArray(
						[
							capitalist$elm_octicons$Octicons$code(
							_Utils_update(
								author$project$Comments$octiconsDefaultOptions,
								{width: 14}))
						])),
					A2(
					elm$html$Html$span,
					_List_fromArray(
						[
							_class(config.cssConfig.toolbarButton),
							ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped, true),
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedS, true)
								])),
							A2(
							elm$html$Html$Attributes$attribute,
							'aria-label',
							A2(author$project$Comments$translate, config.translations, 'comment-editor-tooltip-quote'))
						]),
					_List_fromArray(
						[
							capitalist$elm_octicons$Octicons$quote(
							_Utils_update(
								author$project$Comments$octiconsDefaultOptions,
								{width: 14}))
						])),
					A2(
					elm$html$Html$span,
					_List_fromArray(
						[
							_class(config.cssConfig.toolbarButton),
							ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped, true),
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedS, true)
								])),
							A2(
							elm$html$Html$Attributes$attribute,
							'aria-label',
							A2(author$project$Comments$translate, config.translations, 'comment-editor-tooltip-bulleted-list'))
						]),
					_List_fromArray(
						[
							capitalist$elm_octicons$Octicons$listUnordered(
							_Utils_update(
								author$project$Comments$octiconsDefaultOptions,
								{width: 12}))
						])),
					A2(
					elm$html$Html$span,
					_List_fromArray(
						[
							_class(config.cssConfig.toolbarButton),
							ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped, true),
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedS, true)
								])),
							A2(
							elm$html$Html$Attributes$attribute,
							'aria-label',
							A2(author$project$Comments$translate, config.translations, 'comment-editor-tooltip-numbered-list'))
						]),
					_List_fromArray(
						[
							capitalist$elm_octicons$Octicons$listOrdered(
							_Utils_update(
								author$project$Comments$octiconsDefaultOptions,
								{width: 12}))
						])),
					A2(
					elm$html$Html$span,
					_List_fromArray(
						[
							_class(config.cssConfig.toolbarButton),
							ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTipped, true),
									_Utils_Tuple2(ThinkAlexandria$elm_primer_tooltips$Css$Primer$Tooltips$Selectors$Classes$ToolTippedS, true)
								])),
							A2(
							elm$html$Html$Attributes$attribute,
							'aria-label',
							A2(author$project$Comments$translate, config.translations, 'comment-editor-tooltip-task-list'))
						]),
					_List_fromArray(
						[
							capitalist$elm_octicons$Octicons$tasklist(
							_Utils_update(
								author$project$Comments$octiconsDefaultOptions,
								{width: 16}))
						]))
				]));
	});
var capitalist$elm_octicons$Octicons$markdownPath = 'M14.85,3 L1.15,3 C0.52,3 0,3.52 0,4.15 L0,11.84 C0,12.48 0.52,13 1.15,13 L14.84,13 C15.48,13 15.99,12.48 15.99,11.85 L15.99,4.15 C16,3.52 15.48,3 14.85,3 L14.85,3 Z M9,11 L7,11 L7,8 L5.5,9.92 L4,8 L4,11 L2,11 L2,5 L4,5 L5.5,7 L7,5 L9,5 L9,11 L9,11 Z M11.99,11.5 L9.5,8 L11,8 L11,5 L13,5 L13,8 L14.5,8 L11.99,11.5 L11.99,11.5 Z';
var capitalist$elm_octicons$Octicons$markdown = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$markdownPath, '0 0 16 16', 'markdown');
var elm$html$Html$button = _VirtualDom_node('button');
var elm$html$Html$form = _VirtualDom_node('form');
var elm$html$Html$nav = _VirtualDom_node('nav');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$textarea = _VirtualDom_node('textarea');
var elm$html$Html$Attributes$placeholder = elm$html$Html$Attributes$stringProperty('placeholder');
var elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'focus',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var elm_explorations$markdown$Markdown$toHtmlWith = _Markdown_toHtml;
var author$project$Comments$commentEditor = F4(
	function (config, _n0, selector, draft) {
		var _n1 = _n0.a;
		var leftText = _n1.a;
		var leftDecoder = _n1.b;
		var _n2 = _n0.b;
		var rightText = _n2.a;
		var rightDecoder = _n2.b;
		var toString = config.cssConfig.toString;
		var _class = function (value) {
			return elm$html$Html$Attributes$class(
				config.cssConfig.toString(value));
		};
		var editorBody = draft.showMarkdownPreview ? A3(
			elm_explorations$markdown$Markdown$toHtmlWith,
			{
				defaultHighlighting: elm$core$Maybe$Just('elm'),
				githubFlavored: elm$core$Maybe$Just(
					{breaks: false, tables: false}),
				sanitize: true,
				smartypants: false
			},
			_List_fromArray(
				[
					_class(config.cssConfig.markdownBody)
				]),
			draft.markdown) : A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					_class(config.cssConfig.body)
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$textarea,
					_List_fromArray(
						[
							elm$html$Html$Attributes$tabindex(1),
							elm$html$Html$Attributes$placeholder(
							A2(author$project$Comments$translate, config.translations, 'comment-editor-placeholder')),
							_class(config.cssConfig.textInput),
							elm$html$Html$Events$onInput(
							A2(
								elm$core$Basics$composeL,
								author$project$Comments$Internal,
								author$project$Comments$UpdateCommentDraft(selector))),
							elm$html$Html$Events$onFocus(
							author$project$Comments$Internal(
								author$project$Comments$CreateCommentDraft(selector))),
							elm$html$Html$Attributes$value(draft.markdown)
						]),
					_List_Nil)
				]));
		return A2(
			elm$html$Html$form,
			_List_fromArray(
				[
					_class(config.cssConfig.editor)
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							_class(config.cssConfig.header)
						]),
					_List_fromArray(
						[
							A2(author$project$Comments$viewToolbar, config, draft.showMarkdownPreview),
							A2(
							elm$html$Html$nav,
							_List_fromArray(
								[
									_class(config.cssConfig.horizontalTabNav)
								]),
							_List_fromArray(
								[
									A2(
									elm$html$Html$button,
									_List_fromArray(
										[
											elm$html$Html$Attributes$classList(
											_List_fromArray(
												[
													_Utils_Tuple2(
													toString(config.cssConfig.horizontalTabSelected),
													!draft.showMarkdownPreview),
													_Utils_Tuple2(
													toString(config.cssConfig.horizontalTab),
													true)
												])),
											A2(elm$html$Html$Attributes$attribute, 'role', 'tab'),
											A2(
											elm$html$Html$Events$custom,
											'click',
											elm$json$Json$Decode$succeed(
												{
													message: author$project$Comments$Internal(
														A2(author$project$Comments$PreviewCommentDraft, selector, false)),
													preventDefault: true,
													stopPropagation: true
												}))
										]),
									_List_fromArray(
										[
											elm$html$Html$text(
											A2(author$project$Comments$translate, config.translations, 'comment-editor-tab-write'))
										])),
									A2(
									elm$html$Html$button,
									_List_fromArray(
										[
											elm$html$Html$Attributes$classList(
											_List_fromArray(
												[
													_Utils_Tuple2(
													toString(config.cssConfig.horizontalTabSelected),
													draft.showMarkdownPreview),
													_Utils_Tuple2(
													toString(config.cssConfig.horizontalTab),
													true)
												])),
											A2(elm$html$Html$Attributes$attribute, 'role', 'tab'),
											A2(
											elm$html$Html$Events$custom,
											'click',
											elm$json$Json$Decode$succeed(
												{
													message: author$project$Comments$Internal(
														A2(author$project$Comments$PreviewCommentDraft, selector, true)),
													preventDefault: true,
													stopPropagation: true
												}))
										]),
									_List_fromArray(
										[
											elm$html$Html$text(
											A2(author$project$Comments$translate, config.translations, 'comment-editor-tab-preview'))
										]))
								]))
						])),
					editorBody,
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							_class(config.cssConfig.footer)
						]),
					_List_fromArray(
						[
							A2(
							elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									capitalist$elm_octicons$Octicons$markdown(capitalist$elm_octicons$Octicons$defaultOptions),
									elm$html$Html$text(
									A2(author$project$Comments$translate, config.translations, 'comment-editor-markdown-supported'))
								])),
							A2(
							elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									elm$html$Html$button,
									_List_fromArray(
										[
											elm$html$Html$Attributes$tabindex(3),
											_class(config.cssConfig.button),
											A2(
											elm$html$Html$Events$custom,
											'click',
											A2(
												elm$json$Json$Decode$andThen,
												function (m) {
													return elm$json$Json$Decode$succeed(
														{message: m, preventDefault: true, stopPropagation: true});
												},
												leftDecoder))
										]),
									_List_fromArray(
										[
											elm$html$Html$text(leftText)
										])),
									A2(
									elm$html$Html$button,
									_List_fromArray(
										[
											elm$html$Html$Attributes$tabindex(2),
											_class(config.cssConfig.button),
											A2(
											elm$html$Html$Events$custom,
											'click',
											A2(
												elm$json$Json$Decode$andThen,
												function (m) {
													return elm$json$Json$Decode$succeed(
														{message: m, preventDefault: true, stopPropagation: true});
												},
												rightDecoder))
										]),
									_List_fromArray(
										[
											elm$html$Html$text(rightText)
										]))
								]))
						]))
				]));
	});
var author$project$Comments$newCommentEditor = F2(
	function (config, key) {
		var selector = author$project$Comments$New(key);
		return A3(
			author$project$Comments$commentEditor,
			config,
			_Utils_Tuple2(
				_Utils_Tuple2(
					A2(author$project$Comments$translate, config.translations, 'comment-editor-discard-draft'),
					elm$json$Json$Decode$succeed(
						author$project$Comments$Internal(
							author$project$Comments$DeleteCommentDraft(selector)))),
				_Utils_Tuple2(
					A2(author$project$Comments$translate, config.translations, 'comment-editor-comment'),
					elm$json$Json$Decode$succeed(
						author$project$Comments$NewComment(key)))),
			selector);
	});
var author$project$Comments$DeleteComment = F2(
	function (a, b) {
		return {$: 'DeleteComment', a: a, b: b};
	});
var author$project$Comments$Existing = F2(
	function (a, b) {
		return {$: 'Existing', a: a, b: b};
	});
var author$project$Comments$UpdateComment = F2(
	function (a, b) {
		return {$: 'UpdateComment', a: a, b: b};
	});
var author$project$Comments$updateCommentEditor = F3(
	function (config, key, commentIndex) {
		var selector = A2(author$project$Comments$Existing, key, commentIndex);
		return A3(
			author$project$Comments$commentEditor,
			config,
			_Utils_Tuple2(
				_Utils_Tuple2(
					A2(author$project$Comments$translate, config.translations, 'comment-editor-cancel'),
					elm$json$Json$Decode$succeed(
						author$project$Comments$Internal(
							author$project$Comments$DeleteCommentDraft(selector)))),
				_Utils_Tuple2(
					A2(author$project$Comments$translate, config.translations, 'comment-editor-update-comment'),
					elm$json$Json$Decode$succeed(
						A2(author$project$Comments$UpdateComment, key, commentIndex)))),
			selector);
	});
var capitalist$elm_octicons$Octicons$pencilPath = 'M0,12 L0,15 L3,15 L11,7 L8,4 L0,12 L0,12 Z M3,14 L1,14 L1,12 L2,12 L2,13 L3,13 L3,14 L3,14 Z M13.3,4.7 L12,6 L9,3 L10.3,1.7 C10.69,1.31 11.32,1.31 11.71,1.7 L13.3,3.29 C13.69,3.68 13.69,4.31 13.3,4.7 L13.3,4.7 Z';
var capitalist$elm_octicons$Octicons$pencil = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$pencilPath, '0 0 14 16', 'pencil');
var capitalist$elm_octicons$Octicons$trashcanPath = 'M11,2 L9,2 C9,1.45 8.55,1 8,1 L5,1 C4.45,1 4,1.45 4,2 L2,2 C1.45,2 1,2.45 1,3 L1,4 C1,4.55 1.45,5 2,5 L2,14 C2,14.55 2.45,15 3,15 L10,15 C10.55,15 11,14.55 11,14 L11,5 C11.55,5 12,4.55 12,4 L12,3 C12,2.45 11.55,2 11,2 L11,2 Z M10,14 L3,14 L3,5 L4,5 L4,13 L5,13 L5,5 L6,5 L6,13 L7,13 L7,5 L8,5 L8,13 L9,13 L9,5 L10,5 L10,14 L10,14 Z M11,4 L2,4 L2,3 L11,3 L11,4 L11,4 Z';
var capitalist$elm_octicons$Octicons$trashcan = A3(capitalist$elm_octicons$Octicons$pathIconWithOptions, capitalist$elm_octicons$Octicons$trashcanPath, '0 0 12 16', 'trashcan');
var elm$html$Html$a = _VirtualDom_node('a');
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
var elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return elm$core$Basics$floor(numerator / denominator);
	});
var elm$time$Time$posixToMillis = function (_n0) {
	var millis = _n0.a;
	return millis;
};
var elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var elm$time$Time$toAdjustedMinutes = F2(
	function (_n0, time) {
		var defaultOffset = _n0.a;
		var eras = _n0.b;
		return A3(
			elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				elm$time$Time$flooredDiv,
				elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2(elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		day: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		month: month,
		year: year + ((month <= 2) ? 1 : 0)
	};
};
var elm$time$Time$toDay = F2(
	function (zone, time) {
		return elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).day;
	});
var elm$core$Basics$modBy = _Basics_modBy;
var elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			elm$core$Basics$modBy,
			24,
			A2(
				elm$time$Time$flooredDiv,
				A2(elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var elm$time$Time$toMillis = F2(
	function (_n0, time) {
		return A2(
			elm$core$Basics$modBy,
			1000,
			elm$time$Time$posixToMillis(time));
	});
var elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			elm$core$Basics$modBy,
			60,
			A2(elm$time$Time$toAdjustedMinutes, zone, time));
	});
var elm$time$Time$Apr = {$: 'Apr'};
var elm$time$Time$Aug = {$: 'Aug'};
var elm$time$Time$Dec = {$: 'Dec'};
var elm$time$Time$Feb = {$: 'Feb'};
var elm$time$Time$Jan = {$: 'Jan'};
var elm$time$Time$Jul = {$: 'Jul'};
var elm$time$Time$Jun = {$: 'Jun'};
var elm$time$Time$Mar = {$: 'Mar'};
var elm$time$Time$May = {$: 'May'};
var elm$time$Time$Nov = {$: 'Nov'};
var elm$time$Time$Oct = {$: 'Oct'};
var elm$time$Time$Sep = {$: 'Sep'};
var elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _n0 = elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).month;
		switch (_n0) {
			case 1:
				return elm$time$Time$Jan;
			case 2:
				return elm$time$Time$Feb;
			case 3:
				return elm$time$Time$Mar;
			case 4:
				return elm$time$Time$Apr;
			case 5:
				return elm$time$Time$May;
			case 6:
				return elm$time$Time$Jun;
			case 7:
				return elm$time$Time$Jul;
			case 8:
				return elm$time$Time$Aug;
			case 9:
				return elm$time$Time$Sep;
			case 10:
				return elm$time$Time$Oct;
			case 11:
				return elm$time$Time$Nov;
			default:
				return elm$time$Time$Dec;
		}
	});
var elm$time$Time$toSecond = F2(
	function (_n0, time) {
		return A2(
			elm$core$Basics$modBy,
			60,
			A2(
				elm$time$Time$flooredDiv,
				elm$time$Time$posixToMillis(time),
				1000));
	});
var elm$time$Time$toYear = F2(
	function (zone, time) {
		return elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).year;
	});
var elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var elm$time$Time$utc = A2(elm$time$Time$Zone, 0, _List_Nil);
var rtfeldman$elm_iso8601_date_strings$Iso8601$fromMonth = function (month) {
	switch (month.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)),
			string);
	});
var rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString = F2(
	function (digits, time) {
		return A3(
			elm$core$String$padLeft,
			digits,
			_Utils_chr('0'),
			elm$core$String$fromInt(time));
	});
var rtfeldman$elm_iso8601_date_strings$Iso8601$fromTime = function (time) {
	return A2(
		rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		4,
		A2(elm$time$Time$toYear, elm$time$Time$utc, time)) + ('-' + (A2(
		rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		rtfeldman$elm_iso8601_date_strings$Iso8601$fromMonth(
			A2(elm$time$Time$toMonth, elm$time$Time$utc, time))) + ('-' + (A2(
		rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2(elm$time$Time$toDay, elm$time$Time$utc, time)) + ('T' + (A2(
		rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2(elm$time$Time$toHour, elm$time$Time$utc, time)) + (':' + (A2(
		rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2(elm$time$Time$toMinute, elm$time$Time$utc, time)) + (':' + (A2(
		rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2(elm$time$Time$toSecond, elm$time$Time$utc, time)) + ('.' + (A2(
		rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		3,
		A2(elm$time$Time$toMillis, elm$time$Time$utc, time)) + 'Z'))))))))))));
};
var author$project$Comments$viewComment = F6(
	function (config, state, key, commentIndex, getName, comment) {
		var username = A2(
			elm$core$Maybe$withDefault,
			'Anonymous',
			getName(comment.metadata.createdBy));
		var selector = A2(author$project$Comments$Existing, key, commentIndex);
		var _class = function (value) {
			return elm$html$Html$Attributes$class(
				config.cssConfig.toString(value));
		};
		var _n0 = function (m) {
			if (m.$ === 'Just') {
				var draft = m.a;
				return _Utils_Tuple2(true, draft);
			} else {
				return _Utils_Tuple2(false, author$project$Comments$defaultCommentDraft);
			}
		}(
			A2(
				elm$core$Maybe$andThen,
				elm$core$Dict$get(commentIndex),
				A2(elm$core$Dict$get, key, state.editCommentDrafts)));
		var editing = _n0.a;
		var draftComment = _n0.b;
		return editing ? A2(
			elm$html$Html$map,
			config.toMsg,
			A4(author$project$Comments$updateCommentEditor, config, key, commentIndex, draftComment)) : A2(
			elm$html$Html$map,
			config.toMsg,
			A2(
				elm$html$Html$form,
				_List_fromArray(
					[
						_class(config.cssConfig.editor)
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								_class(config.cssConfig.header)
							]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										elm$html$Html$a,
										_List_Nil,
										_List_fromArray(
											[
												elm$html$Html$text(username)
											])),
										elm$html$Html$text(' commented '),
										elm$html$Html$text(
										rtfeldman$elm_iso8601_date_strings$Iso8601$fromTime(comment.metadata.createdTimestamp))
									])),
								A2(
								elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										elm$html$Html$button,
										_List_fromArray(
											[
												A2(
												elm$html$Html$Events$custom,
												'click',
												elm$json$Json$Decode$succeed(
													{
														message: author$project$Comments$Internal(
															author$project$Comments$CreateCommentDraft(selector)),
														preventDefault: true,
														stopPropagation: true
													})),
												_class(config.cssConfig.toolbarButton)
											]),
										_List_fromArray(
											[
												capitalist$elm_octicons$Octicons$pencil(
												_Utils_update(
													author$project$Comments$octiconsDefaultOptions,
													{width: 14}))
											])),
										A2(
										elm$html$Html$button,
										_List_fromArray(
											[
												A2(
												elm$html$Html$Events$custom,
												'click',
												elm$json$Json$Decode$succeed(
													{
														message: A2(author$project$Comments$DeleteComment, key, commentIndex),
														preventDefault: true,
														stopPropagation: true
													})),
												_class(config.cssConfig.toolbarButton)
											]),
										_List_fromArray(
											[
												capitalist$elm_octicons$Octicons$trashcan(
												_Utils_update(
													author$project$Comments$octiconsDefaultOptions,
													{width: 12}))
											]))
									]))
							])),
						A3(
						elm_explorations$markdown$Markdown$toHtmlWith,
						{
							defaultHighlighting: elm$core$Maybe$Just('elm'),
							githubFlavored: elm$core$Maybe$Just(
								{breaks: false, tables: false}),
							sanitize: true,
							smartypants: false
						},
						_List_fromArray(
							[
								_class(config.cssConfig.markdownBody)
							]),
						comment.markdown)
					])));
	});
var author$project$Comments$viewCommentList = F5(
	function (config, state, key, getName, comments) {
		var tailEditor = function () {
			var _n0 = A2(elm$core$Dict$get, key, state.newCommentDrafts);
			if (_n0.$ === 'Just') {
				var draft = _n0.a;
				return _List_fromArray(
					[
						A2(
						elm$html$Html$map,
						config.toMsg,
						A3(author$project$Comments$newCommentEditor, config, key, draft))
					]);
			} else {
				return _List_fromArray(
					[
						A2(
						elm$html$Html$map,
						config.toMsg,
						A3(author$project$Comments$newCommentEditor, config, key, author$project$Comments$defaultCommentDraft))
					]);
			}
		}();
		var _class = function (value) {
			return elm$html$Html$Attributes$class(
				config.cssConfig.toString(value));
		};
		return _Utils_ap(
			elm$core$List$concat(
				A2(
					elm$core$List$indexedMap,
					F2(
						function (index, value) {
							return value.metadata.isDeleted ? _List_fromArray(
								[
									elm$html$Html$text('')
								]) : _List_fromArray(
								[
									A6(author$project$Comments$viewComment, config, state, key, index, getName, value),
									A2(
									elm$html$Html$div,
									_List_fromArray(
										[
											_class(config.cssConfig.spacer)
										]),
									_List_Nil)
								]);
						}),
					comments)),
			tailEditor);
	});
var elm$html$Html$li = _VirtualDom_node('li');
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var author$project$Example$viewPost = F3(
	function (_n0, postIndex, post) {
		var translations = _n0.translations;
		var commentState = _n0.commentState;
		return A2(
			elm$html$Html$li,
			_List_fromArray(
				[
					A2(elm$html$Html$Attributes$style, 'max-width', '700px'),
					A2(elm$html$Html$Attributes$style, 'background-color', '#eee'),
					A2(elm$html$Html$Attributes$style, 'padding', '10px'),
					A2(elm$html$Html$Attributes$style, 'margin', '10px 0px'),
					A2(elm$html$Html$Attributes$style, 'border', '1px solid #ddd')
				]),
			A5(
				author$project$Comments$viewCommentList,
				{cssConfig: author$project$Comments$Css$defaultCssConfig, toMsg: author$project$Example$CommentsMsg, translations: translations},
				commentState,
				postIndex,
				function (x) {
					return elm$core$Maybe$Just(x);
				},
				elm$core$Array$toList(post.comments)));
	});
var elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var elm$core$Array$indexedMap = F2(
	function (func, _n0) {
		var len = _n0.a;
		var tree = _n0.c;
		var tail = _n0.d;
		var initialBuilder = {
			nodeList: _List_Nil,
			nodeListSize: 0,
			tail: A3(
				elm$core$Elm$JsArray$indexedMap,
				func,
				elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.nodeListSize * elm$core$Array$branchFactor;
					var mappedLeaf = elm$core$Array$Leaf(
						A3(elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						nodeList: A2(elm$core$List$cons, mappedLeaf, builder.nodeList),
						nodeListSize: builder.nodeListSize + 1,
						tail: builder.tail
					};
				}
			});
		return A2(
			elm$core$Array$builderToArray,
			true,
			A3(elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var elm$html$Html$h3 = _VirtualDom_node('h3');
var elm$html$Html$hr = _VirtualDom_node('hr');
var elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$node = elm$virtual_dom$VirtualDom$node;
var elm$html$Html$p = _VirtualDom_node('p');
var elm$html$Html$ul = _VirtualDom_node('ul');
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Example$view = function (model) {
	return {
		body: _List_fromArray(
			[
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						A2(elm$html$Html$Attributes$style, 'margin', '20px auto'),
						A2(elm$html$Html$Attributes$style, 'padding', '20px'),
						A2(elm$html$Html$Attributes$style, 'max-width', '1080px')
					]),
				_List_fromArray(
					[
						A3(
						elm$html$Html$node,
						'style',
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text(
								ThinkAlexandria$css_in_elm$Css$compile(
									_List_fromArray(
										[author$project$Comments$Css$exampleStyleSheet])).css)
							])),
						A2(
						elm$html$Html$h3,
						_List_Nil,
						_List_fromArray(
							[
								elm$html$Html$text('viewCommentList Demo')
							])),
						A2(
						elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text('You can edit comments on multiple posts simultaneously')
									])),
								A2(
								elm$html$Html$button,
								_List_fromArray(
									[
										elm$html$Html$Events$onClick(author$project$Example$NewPost)
									]),
								_List_fromArray(
									[
										elm$html$Html$text('New Post')
									]))
							])),
						A2(elm$html$Html$hr, _List_Nil, _List_Nil),
						A2(
						elm$html$Html$ul,
						_List_Nil,
						elm$core$Array$toList(
							A2(
								elm$core$Array$indexedMap,
								author$project$Example$viewPost(model),
								model.feed)))
					]))
			]),
		title: ''
	};
};
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$core$String$toInt = _String_toInt;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$document = _Browser_document;
var author$project$Example$main = elm$browser$Browser$document(
	{init: author$project$Example$init, subscriptions: author$project$Example$subscriptions, update: author$project$Example$update, view: author$project$Example$view});
_Platform_export({'Example':{'init':author$project$Example$main(
	elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));/*! highlight.js v9.11.0 | BSD3 License | git.io/hljslicense */
!function(e){var n="object"==typeof window&&window||"object"==typeof self&&self;"undefined"!=typeof exports?e(exports):n&&(n.hljs=e({}),"function"==typeof define&&define.amd&&define([],function(){return n.hljs}))}(function(e){function n(e){return e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;")}function t(e){return e.nodeName.toLowerCase()}function r(e,n){var t=e&&e.exec(n);return t&&0===t.index}function a(e){return k.test(e)}function i(e){var n,t,r,i,o=e.className+" ";if(o+=e.parentNode?e.parentNode.className:"",t=B.exec(o))return w(t[1])?t[1]:"no-highlight";for(o=o.split(/\s+/),n=0,r=o.length;r>n;n++)if(i=o[n],a(i)||w(i))return i}function o(e){var n,t={},r=Array.prototype.slice.call(arguments,1);for(n in e)t[n]=e[n];return r.forEach(function(e){for(n in e)t[n]=e[n]}),t}function u(e){var n=[];return function r(e,a){for(var i=e.firstChild;i;i=i.nextSibling)3===i.nodeType?a+=i.nodeValue.length:1===i.nodeType&&(n.push({event:"start",offset:a,node:i}),a=r(i,a),t(i).match(/br|hr|img|input/)||n.push({event:"stop",offset:a,node:i}));return a}(e,0),n}function c(e,r,a){function i(){return e.length&&r.length?e[0].offset!==r[0].offset?e[0].offset<r[0].offset?e:r:"start"===r[0].event?e:r:e.length?e:r}function o(e){function r(e){return" "+e.nodeName+'="'+n(e.value).replace('"',"&quot;")+'"'}s+="<"+t(e)+E.map.call(e.attributes,r).join("")+">"}function u(e){s+="</"+t(e)+">"}function c(e){("start"===e.event?o:u)(e.node)}for(var l=0,s="",f=[];e.length||r.length;){var g=i();if(s+=n(a.substring(l,g[0].offset)),l=g[0].offset,g===e){f.reverse().forEach(u);do c(g.splice(0,1)[0]),g=i();while(g===e&&g.length&&g[0].offset===l);f.reverse().forEach(o)}else"start"===g[0].event?f.push(g[0].node):f.pop(),c(g.splice(0,1)[0])}return s+n(a.substr(l))}function l(e){return e.v&&!e.cached_variants&&(e.cached_variants=e.v.map(function(n){return o(e,{v:null},n)})),e.cached_variants||e.eW&&[o(e)]||[e]}function s(e){function n(e){return e&&e.source||e}function t(t,r){return new RegExp(n(t),"m"+(e.cI?"i":"")+(r?"g":""))}function r(a,i){if(!a.compiled){if(a.compiled=!0,a.k=a.k||a.bK,a.k){var o={},u=function(n,t){e.cI&&(t=t.toLowerCase()),t.split(" ").forEach(function(e){var t=e.split("|");o[t[0]]=[n,t[1]?Number(t[1]):1]})};"string"==typeof a.k?u("keyword",a.k):x(a.k).forEach(function(e){u(e,a.k[e])}),a.k=o}a.lR=t(a.l||/\w+/,!0),i&&(a.bK&&(a.b="\\b("+a.bK.split(" ").join("|")+")\\b"),a.b||(a.b=/\B|\b/),a.bR=t(a.b),a.e||a.eW||(a.e=/\B|\b/),a.e&&(a.eR=t(a.e)),a.tE=n(a.e)||"",a.eW&&i.tE&&(a.tE+=(a.e?"|":"")+i.tE)),a.i&&(a.iR=t(a.i)),null==a.r&&(a.r=1),a.c||(a.c=[]),a.c=Array.prototype.concat.apply([],a.c.map(function(e){return l("self"===e?a:e)})),a.c.forEach(function(e){r(e,a)}),a.starts&&r(a.starts,i);var c=a.c.map(function(e){return e.bK?"\\.?("+e.b+")\\.?":e.b}).concat([a.tE,a.i]).map(n).filter(Boolean);a.t=c.length?t(c.join("|"),!0):{exec:function(){return null}}}}r(e)}function f(e,t,a,i){function o(e,n){var t,a;for(t=0,a=n.c.length;a>t;t++)if(r(n.c[t].bR,e))return n.c[t]}function u(e,n){if(r(e.eR,n)){for(;e.endsParent&&e.parent;)e=e.parent;return e}return e.eW?u(e.parent,n):void 0}function c(e,n){return!a&&r(n.iR,e)}function l(e,n){var t=N.cI?n[0].toLowerCase():n[0];return e.k.hasOwnProperty(t)&&e.k[t]}function p(e,n,t,r){var a=r?"":I.classPrefix,i='<span class="'+a,o=t?"":C;return i+=e+'">',i+n+o}function h(){var e,t,r,a;if(!E.k)return n(k);for(a="",t=0,E.lR.lastIndex=0,r=E.lR.exec(k);r;)a+=n(k.substring(t,r.index)),e=l(E,r),e?(B+=e[1],a+=p(e[0],n(r[0]))):a+=n(r[0]),t=E.lR.lastIndex,r=E.lR.exec(k);return a+n(k.substr(t))}function d(){var e="string"==typeof E.sL;if(e&&!y[E.sL])return n(k);var t=e?f(E.sL,k,!0,x[E.sL]):g(k,E.sL.length?E.sL:void 0);return E.r>0&&(B+=t.r),e&&(x[E.sL]=t.top),p(t.language,t.value,!1,!0)}function b(){L+=null!=E.sL?d():h(),k=""}function v(e){L+=e.cN?p(e.cN,"",!0):"",E=Object.create(e,{parent:{value:E}})}function m(e,n){if(k+=e,null==n)return b(),0;var t=o(n,E);if(t)return t.skip?k+=n:(t.eB&&(k+=n),b(),t.rB||t.eB||(k=n)),v(t,n),t.rB?0:n.length;var r=u(E,n);if(r){var a=E;a.skip?k+=n:(a.rE||a.eE||(k+=n),b(),a.eE&&(k=n));do E.cN&&(L+=C),E.skip||(B+=E.r),E=E.parent;while(E!==r.parent);return r.starts&&v(r.starts,""),a.rE?0:n.length}if(c(n,E))throw new Error('Illegal lexeme "'+n+'" for mode "'+(E.cN||"<unnamed>")+'"');return k+=n,n.length||1}var N=w(e);if(!N)throw new Error('Unknown language: "'+e+'"');s(N);var R,E=i||N,x={},L="";for(R=E;R!==N;R=R.parent)R.cN&&(L=p(R.cN,"",!0)+L);var k="",B=0;try{for(var M,j,O=0;;){if(E.t.lastIndex=O,M=E.t.exec(t),!M)break;j=m(t.substring(O,M.index),M[0]),O=M.index+j}for(m(t.substr(O)),R=E;R.parent;R=R.parent)R.cN&&(L+=C);return{r:B,value:L,language:e,top:E}}catch(T){if(T.message&&-1!==T.message.indexOf("Illegal"))return{r:0,value:n(t)};throw T}}function g(e,t){t=t||I.languages||x(y);var r={r:0,value:n(e)},a=r;return t.filter(w).forEach(function(n){var t=f(n,e,!1);t.language=n,t.r>a.r&&(a=t),t.r>r.r&&(a=r,r=t)}),a.language&&(r.second_best=a),r}function p(e){return I.tabReplace||I.useBR?e.replace(M,function(e,n){return I.useBR&&"\n"===e?"<br>":I.tabReplace?n.replace(/\t/g,I.tabReplace):""}):e}function h(e,n,t){var r=n?L[n]:t,a=[e.trim()];return e.match(/\bhljs\b/)||a.push("hljs"),-1===e.indexOf(r)&&a.push(r),a.join(" ").trim()}function d(e){var n,t,r,o,l,s=i(e);a(s)||(I.useBR?(n=document.createElementNS("http://www.w3.org/1999/xhtml","div"),n.innerHTML=e.innerHTML.replace(/\n/g,"").replace(/<br[ \/]*>/g,"\n")):n=e,l=n.textContent,r=s?f(s,l,!0):g(l),t=u(n),t.length&&(o=document.createElementNS("http://www.w3.org/1999/xhtml","div"),o.innerHTML=r.value,r.value=c(t,u(o),l)),r.value=p(r.value),e.innerHTML=r.value,e.className=h(e.className,s,r.language),e.result={language:r.language,re:r.r},r.second_best&&(e.second_best={language:r.second_best.language,re:r.second_best.r}))}function b(e){I=o(I,e)}function v(){if(!v.called){v.called=!0;var e=document.querySelectorAll("pre code");E.forEach.call(e,d)}}function m(){addEventListener("DOMContentLoaded",v,!1),addEventListener("load",v,!1)}function N(n,t){var r=y[n]=t(e);r.aliases&&r.aliases.forEach(function(e){L[e]=n})}function R(){return x(y)}function w(e){return e=(e||"").toLowerCase(),y[e]||y[L[e]]}var E=[],x=Object.keys,y={},L={},k=/^(no-?highlight|plain|text)$/i,B=/\blang(?:uage)?-([\w-]+)\b/i,M=/((^(<[^>]+>|\t|)+|(?:\n)))/gm,C="</span>",I={classPrefix:"hljs-",tabReplace:null,useBR:!1,languages:void 0};return e.highlight=f,e.highlightAuto=g,e.fixMarkup=p,e.highlightBlock=d,e.configure=b,e.initHighlighting=v,e.initHighlightingOnLoad=m,e.registerLanguage=N,e.listLanguages=R,e.getLanguage=w,e.inherit=o,e.IR="[a-zA-Z]\\w*",e.UIR="[a-zA-Z_]\\w*",e.NR="\\b\\d+(\\.\\d+)?",e.CNR="(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)",e.BNR="\\b(0b[01]+)",e.RSR="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~",e.BE={b:"\\\\[\\s\\S]",r:0},e.ASM={cN:"string",b:"'",e:"'",i:"\\n",c:[e.BE]},e.QSM={cN:"string",b:'"',e:'"',i:"\\n",c:[e.BE]},e.PWM={b:/\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/},e.C=function(n,t,r){var a=e.inherit({cN:"comment",b:n,e:t,c:[]},r||{});return a.c.push(e.PWM),a.c.push({cN:"doctag",b:"(?:TODO|FIXME|NOTE|BUG|XXX):",r:0}),a},e.CLCM=e.C("//","$"),e.CBCM=e.C("/\\*","\\*/"),e.HCM=e.C("#","$"),e.NM={cN:"number",b:e.NR,r:0},e.CNM={cN:"number",b:e.CNR,r:0},e.BNM={cN:"number",b:e.BNR,r:0},e.CSSNM={cN:"number",b:e.NR+"(%|em|ex|ch|rem|vw|vh|vmin|vmax|cm|mm|in|pt|pc|px|deg|grad|rad|turn|s|ms|Hz|kHz|dpi|dpcm|dppx)?",r:0},e.RM={cN:"regexp",b:/\//,e:/\/[gimuy]*/,i:/\n/,c:[e.BE,{b:/\[/,e:/\]/,r:0,c:[e.BE]}]},e.TM={cN:"title",b:e.IR,r:0},e.UTM={cN:"title",b:e.UIR,r:0},e.METHOD_GUARD={b:"\\.\\s*"+e.UIR,r:0},e});hljs.registerLanguage("xml",function(s){var e="[A-Za-z0-9\\._:-]+",t={eW:!0,i:/</,r:0,c:[{cN:"attr",b:e,r:0},{b:/=\s*/,r:0,c:[{cN:"string",endsParent:!0,v:[{b:/"/,e:/"/},{b:/'/,e:/'/},{b:/[^\s"'=<>`]+/}]}]}]};return{aliases:["html","xhtml","rss","atom","xjb","xsd","xsl","plist"],cI:!0,c:[{cN:"meta",b:"<!DOCTYPE",e:">",r:10,c:[{b:"\\[",e:"\\]"}]},s.C("<!--","-->",{r:10}),{b:"<\\!\\[CDATA\\[",e:"\\]\\]>",r:10},{b:/<\?(php)?/,e:/\?>/,sL:"php",c:[{b:"/\\*",e:"\\*/",skip:!0}]},{cN:"tag",b:"<style(?=\\s|>|$)",e:">",k:{name:"style"},c:[t],starts:{e:"</style>",rE:!0,sL:["css","xml"]}},{cN:"tag",b:"<script(?=\\s|>|$)",e:">",k:{name:"script"},c:[t],starts:{e:"</script>",rE:!0,sL:["actionscript","javascript","handlebars","xml"]}},{cN:"meta",v:[{b:/<\?xml/,e:/\?>/,r:10},{b:/<\?\w+/,e:/\?>/}]},{cN:"tag",b:"</?",e:"/?>",c:[{cN:"name",b:/[^\/><\s]+/,r:0},t]}]}});hljs.registerLanguage("lua",function(e){var t="\\[=*\\[",a="\\]=*\\]",r={b:t,e:a,c:["self"]},n=[e.C("--(?!"+t+")","$"),e.C("--"+t,a,{c:[r],r:10})];return{l:e.UIR,k:{literal:"true false nil",keyword:"and break do else elseif end for goto if in local not or repeat return then until while",built_in:"_G _ENV _VERSION __index __newindex __mode __call __metatable __tostring __len __gc __add __sub __mul __div __mod __pow __concat __unm __eq __lt __le assert collectgarbage dofile error getfenv getmetatable ipairs load loadfile loadstringmodule next pairs pcall print rawequal rawget rawset require select setfenvsetmetatable tonumber tostring type unpack xpcall arg selfcoroutine resume yield status wrap create running debug getupvalue debug sethook getmetatable gethook setmetatable setlocal traceback setfenv getinfo setupvalue getlocal getregistry getfenv io lines write close flush open output type read stderr stdin input stdout popen tmpfile math log max acos huge ldexp pi cos tanh pow deg tan cosh sinh random randomseed frexp ceil floor rad abs sqrt modf asin min mod fmod log10 atan2 exp sin atan os exit setlocale date getenv difftime remove time clock tmpname rename execute package preload loadlib loaded loaders cpath config path seeall string sub upper len gfind rep find match char dump gmatch reverse byte format gsub lower table setn insert getn foreachi maxn foreach concat sort remove"},c:n.concat([{cN:"function",bK:"function",e:"\\)",c:[e.inherit(e.TM,{b:"([_a-zA-Z]\\w*\\.)*([_a-zA-Z]\\w*:)?[_a-zA-Z]\\w*"}),{cN:"params",b:"\\(",eW:!0,c:n}].concat(n)},e.CNM,e.ASM,e.QSM,{cN:"string",b:t,e:a,c:[r],r:5}])}});hljs.registerLanguage("css",function(e){var c="[a-zA-Z-][a-zA-Z0-9_-]*",t={b:/[A-Z\_\.\-]+\s*:/,rB:!0,e:";",eW:!0,c:[{cN:"attribute",b:/\S/,e:":",eE:!0,starts:{eW:!0,eE:!0,c:[{b:/[\w-]+\(/,rB:!0,c:[{cN:"built_in",b:/[\w-]+/},{b:/\(/,e:/\)/,c:[e.ASM,e.QSM]}]},e.CSSNM,e.QSM,e.ASM,e.CBCM,{cN:"number",b:"#[0-9A-Fa-f]+"},{cN:"meta",b:"!important"}]}}]};return{cI:!0,i:/[=\/|'\$]/,c:[e.CBCM,{cN:"selector-id",b:/#[A-Za-z0-9_-]+/},{cN:"selector-class",b:/\.[A-Za-z0-9_-]+/},{cN:"selector-attr",b:/\[/,e:/\]/,i:"$"},{cN:"selector-pseudo",b:/:(:)?[a-zA-Z0-9\_\-\+\(\)"'.]+/},{b:"@(font-face|page)",l:"[a-z-]+",k:"font-face page"},{b:"@",e:"[{;]",i:/:/,c:[{cN:"keyword",b:/\w+/},{b:/\s/,eW:!0,eE:!0,r:0,c:[e.ASM,e.QSM,e.CSSNM]}]},{cN:"selector-tag",b:c,r:0},{b:"{",e:"}",i:/\S/,c:[e.CBCM,t]}]}});hljs.registerLanguage("makefile",function(e){var i={cN:"variable",v:[{b:"\\$\\("+e.UIR+"\\)",c:[e.BE]},{b:/\$[@%<?\^\+\*]/}]},r={cN:"string",b:/"/,e:/"/,c:[e.BE,i]},a={cN:"variable",b:/\$\([\w-]+\s/,e:/\)/,k:{built_in:"subst patsubst strip findstring filter filter-out sort word wordlist firstword lastword dir notdir suffix basename addsuffix addprefix join wildcard realpath abspath error warning shell origin flavor foreach if or and call eval file value"},c:[i]},n={b:"^"+e.UIR+"\\s*[:+?]?=",i:"\\n",rB:!0,c:[{b:"^"+e.UIR,e:"[:+?]?=",eE:!0}]},t={cN:"meta",b:/^\.PHONY:/,e:/$/,k:{"meta-keyword":".PHONY"},l:/[\.\w]+/},l={cN:"section",b:/^[^\s]+:/,e:/$/,c:[i]};return{aliases:["mk","mak"],k:"define endef undefine ifdef ifndef ifeq ifneq else endif include -include sinclude override export unexport private vpath",l:/[\w-]+/,c:[e.HCM,i,r,a,n,t,l]}});hljs.registerLanguage("rust",function(e){var t="([ui](8|16|32|64|128|size)|f(32|64))?",r="alignof as be box break const continue crate do else enum extern false fn for if impl in let loop match mod mut offsetof once priv proc pub pure ref return self Self sizeof static struct super trait true type typeof unsafe unsized use virtual while where yield move default",n="drop i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize f32 f64 str char bool Box Option Result String Vec Copy Send Sized Sync Drop Fn FnMut FnOnce ToOwned Clone Debug PartialEq PartialOrd Eq Ord AsRef AsMut Into From Default Iterator Extend IntoIterator DoubleEndedIterator ExactSizeIterator SliceConcatExt ToString assert! assert_eq! bitflags! bytes! cfg! col! concat! concat_idents! debug_assert! debug_assert_eq! env! panic! file! format! format_args! include_bin! include_str! line! local_data_key! module_path! option_env! print! println! select! stringify! try! unimplemented! unreachable! vec! write! writeln! macro_rules! assert_ne! debug_assert_ne!";return{aliases:["rs"],k:{keyword:r,literal:"true false Some None Ok Err",built_in:n},l:e.IR+"!?",i:"</",c:[e.CLCM,e.C("/\\*","\\*/",{c:["self"]}),e.inherit(e.QSM,{b:/b?"/,i:null}),{cN:"string",v:[{b:/r(#*)".*?"\1(?!#)/},{b:/b?'\\?(x\w{2}|u\w{4}|U\w{8}|.)'/}]},{cN:"symbol",b:/'[a-zA-Z_][a-zA-Z0-9_]*/},{cN:"number",v:[{b:"\\b0b([01_]+)"+t},{b:"\\b0o([0-7_]+)"+t},{b:"\\b0x([A-Fa-f0-9_]+)"+t},{b:"\\b(\\d[\\d_]*(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?)"+t}],r:0},{cN:"function",bK:"fn",e:"(\\(|<)",eE:!0,c:[e.UTM]},{cN:"meta",b:"#\\!?\\[",e:"\\]",c:[{cN:"meta-string",b:/"/,e:/"/}]},{cN:"class",bK:"type",e:";",c:[e.inherit(e.UTM,{endsParent:!0})],i:"\\S"},{cN:"class",bK:"trait enum struct union",e:"{",c:[e.inherit(e.UTM,{endsParent:!0})],i:"[\\w\\d]"},{b:e.IR+"::",k:{built_in:n}},{b:"->"}]}});hljs.registerLanguage("markdown",function(e){return{aliases:["md","mkdown","mkd"],c:[{cN:"section",v:[{b:"^#{1,6}",e:"$"},{b:"^.+?\\n[=-]{2,}$"}]},{b:"<",e:">",sL:"xml",r:0},{cN:"bullet",b:"^([*+-]|(\\d+\\.))\\s+"},{cN:"strong",b:"[*_]{2}.+?[*_]{2}"},{cN:"emphasis",v:[{b:"\\*.+?\\*"},{b:"_.+?_",r:0}]},{cN:"quote",b:"^>\\s+",e:"$"},{cN:"code",v:[{b:"^```w*s*$",e:"^```s*$"},{b:"`.+?`"},{b:"^( {4}|	)",e:"$",r:0}]},{b:"^[-\\*]{3,}",e:"$"},{b:"\\[.+?\\][\\(\\[].*?[\\)\\]]",rB:!0,c:[{cN:"string",b:"\\[",e:"\\]",eB:!0,rE:!0,r:0},{cN:"link",b:"\\]\\(",e:"\\)",eB:!0,eE:!0},{cN:"symbol",b:"\\]\\[",e:"\\]",eB:!0,eE:!0}],r:10},{b:/^\[[^\n]+\]:/,rB:!0,c:[{cN:"symbol",b:/\[/,e:/\]/,eB:!0,eE:!0},{cN:"link",b:/:\s*/,e:/$/,eB:!0}]}]}});hljs.registerLanguage("diff",function(e){return{aliases:["patch"],c:[{cN:"meta",r:10,v:[{b:/^@@ +\-\d+,\d+ +\+\d+,\d+ +@@$/},{b:/^\*\*\* +\d+,\d+ +\*\*\*\*$/},{b:/^\-\-\- +\d+,\d+ +\-\-\-\-$/}]},{cN:"comment",v:[{b:/Index: /,e:/$/},{b:/={3,}/,e:/$/},{b:/^\-{3}/,e:/$/},{b:/^\*{3} /,e:/$/},{b:/^\+{3}/,e:/$/},{b:/\*{5}/,e:/\*{5}$/}]},{cN:"addition",b:"^\\+",e:"$"},{cN:"deletion",b:"^\\-",e:"$"},{cN:"addition",b:"^\\!",e:"$"}]}});hljs.registerLanguage("cal",function(e){var r="div mod in and or not xor asserterror begin case do downto else end exit for if of repeat then to until while with var",t="false true",c=[e.CLCM,e.C(/\{/,/\}/,{r:0}),e.C(/\(\*/,/\*\)/,{r:10})],n={cN:"string",b:/'/,e:/'/,c:[{b:/''/}]},o={cN:"string",b:/(#\d+)+/},a={cN:"number",b:"\\b\\d+(\\.\\d+)?(DT|D|T)",r:0},i={cN:"string",b:'"',e:'"'},d={cN:"function",bK:"procedure",e:/[:;]/,k:"procedure|10",c:[e.TM,{cN:"params",b:/\(/,e:/\)/,k:r,c:[n,o]}].concat(c)},s={cN:"class",b:"OBJECT (Table|Form|Report|Dataport|Codeunit|XMLport|MenuSuite|Page|Query) (\\d+) ([^\\r\\n]+)",rB:!0,c:[e.TM,d]};return{cI:!0,k:{keyword:r,literal:t},i:/\/\*/,c:[n,o,a,i,e.NM,s,d]}});hljs.registerLanguage("php",function(e){var c={b:"\\$+[a-zA-Z_-ÿ][a-zA-Z0-9_-ÿ]*"},i={cN:"meta",b:/<\?(php)?|\?>/},t={cN:"string",c:[e.BE,i],v:[{b:'b"',e:'"'},{b:"b'",e:"'"},e.inherit(e.ASM,{i:null}),e.inherit(e.QSM,{i:null})]},a={v:[e.BNM,e.CNM]};return{aliases:["php3","php4","php5","php6"],cI:!0,k:"and include_once list abstract global private echo interface as static endswitch array null if endwhile or const for endforeach self var while isset public protected exit foreach throw elseif include __FILE__ empty require_once do xor return parent clone use __CLASS__ __LINE__ else break print eval new catch __METHOD__ case exception default die require __FUNCTION__ enddeclare final try switch continue endfor endif declare unset true false trait goto instanceof insteadof __DIR__ __NAMESPACE__ yield finally",c:[e.HCM,e.C("//","$",{c:[i]}),e.C("/\\*","\\*/",{c:[{cN:"doctag",b:"@[A-Za-z]+"}]}),e.C("__halt_compiler.+?;",!1,{eW:!0,k:"__halt_compiler",l:e.UIR}),{cN:"string",b:/<<<['"]?\w+['"]?$/,e:/^\w+;?$/,c:[e.BE,{cN:"subst",v:[{b:/\$\w+/},{b:/\{\$/,e:/\}/}]}]},i,{cN:"keyword",b:/\$this\b/},c,{b:/(::|->)+[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*/},{cN:"function",bK:"function",e:/[;{]/,eE:!0,i:"\\$|\\[|%",c:[e.UTM,{cN:"params",b:"\\(",e:"\\)",c:["self",c,e.CBCM,t,a]}]},{cN:"class",bK:"class interface",e:"{",eE:!0,i:/[:\(\$"]/,c:[{bK:"extends implements"},e.UTM]},{bK:"namespace",e:";",i:/[\.']/,c:[e.UTM]},{bK:"use",e:";",c:[e.UTM]},{b:"=>"},t,a]}});hljs.registerLanguage("bash",function(e){var t={cN:"variable",v:[{b:/\$[\w\d#@][\w\d_]*/},{b:/\$\{(.*?)}/}]},s={cN:"string",b:/"/,e:/"/,c:[e.BE,t,{cN:"variable",b:/\$\(/,e:/\)/,c:[e.BE]}]},a={cN:"string",b:/'/,e:/'/};return{aliases:["sh","zsh"],l:/-?[a-z\._]+/,k:{keyword:"if then else elif fi for while in do done case esac function",literal:"true false",built_in:"break cd continue eval exec exit export getopts hash pwd readonly return shift test times trap umask unset alias bind builtin caller command declare echo enable help let local logout mapfile printf read readarray source type typeset ulimit unalias set shopt autoload bg bindkey bye cap chdir clone comparguments compcall compctl compdescribe compfiles compgroups compquote comptags comptry compvalues dirs disable disown echotc echoti emulate fc fg float functions getcap getln history integer jobs kill limit log noglob popd print pushd pushln rehash sched setcap setopt stat suspend ttyctl unfunction unhash unlimit unsetopt vared wait whence where which zcompile zformat zftp zle zmodload zparseopts zprof zpty zregexparse zsocket zstyle ztcp",_:"-ne -eq -lt -gt -f -d -e -s -l -a"},c:[{cN:"meta",b:/^#![^\n]+sh\s*$/,r:10},{cN:"function",b:/\w[\w\d_]*\s*\(\s*\)\s*\{/,rB:!0,c:[e.inherit(e.TM,{b:/\w[\w\d_]*/})],r:0},e.HCM,s,a,t]}});hljs.registerLanguage("json",function(e){var i={literal:"true false null"},n=[e.QSM,e.CNM],r={e:",",eW:!0,eE:!0,c:n,k:i},t={b:"{",e:"}",c:[{cN:"attr",b:/"/,e:/"/,c:[e.BE],i:"\\n"},e.inherit(r,{b:/:/})],i:"\\S"},c={b:"\\[",e:"\\]",c:[e.inherit(r)],i:"\\S"};return n.splice(n.length,0,t,c),{c:n,k:i,i:"\\S"}});hljs.registerLanguage("elm",function(e){var i={v:[e.C("--","$"),e.C("{-","-}",{c:["self"]})]},t={cN:"type",b:"\\b[A-Z][\\w']*",r:0},c={b:"\\(",e:"\\)",i:'"',c:[{cN:"type",b:"\\b[A-Z][\\w]*(\\((\\.\\.|,|\\w+)\\))?"},i]},n={b:"{",e:"}",c:c.c};return{k:"let in if then else case of where module import exposing type alias as infix infixl infixr port effect command subscription",c:[{bK:"port effect module",e:"exposing",k:"port effect module where command subscription exposing",c:[c,i],i:"\\W\\.|;"},{b:"import",e:"$",k:"import as exposing",c:[c,i],i:"\\W\\.|;"},{b:"type",e:"$",k:"type alias",c:[t,c,n,i]},{bK:"infix infixl infixr",e:"$",c:[e.CNM,i]},{b:"port",e:"$",k:"port",c:[i]},e.QSM,e.CNM,t,e.inherit(e.TM,{b:"^[_a-z][\\w']*"}),i,{b:"->|<-"}],i:/;/}});hljs.registerLanguage("ruby",function(e){var b="[a-zA-Z_]\\w*[!?=]?|[-+~]\\@|<<|>>|=~|===?|<=>|[<>]=?|\\*\\*|[-/+%^&*~`|]|\\[\\]=?",r={keyword:"and then defined module in return redo if BEGIN retry end for self when next until do begin unless END rescue else break undef not super class case require yield alias while ensure elsif or include attr_reader attr_writer attr_accessor",literal:"true false nil"},c={cN:"doctag",b:"@[A-Za-z]+"},a={b:"#<",e:">"},s=[e.C("#","$",{c:[c]}),e.C("^\\=begin","^\\=end",{c:[c],r:10}),e.C("^__END__","\\n$")],n={cN:"subst",b:"#\\{",e:"}",k:r},t={cN:"string",c:[e.BE,n],v:[{b:/'/,e:/'/},{b:/"/,e:/"/},{b:/`/,e:/`/},{b:"%[qQwWx]?\\(",e:"\\)"},{b:"%[qQwWx]?\\[",e:"\\]"},{b:"%[qQwWx]?{",e:"}"},{b:"%[qQwWx]?<",e:">"},{b:"%[qQwWx]?/",e:"/"},{b:"%[qQwWx]?%",e:"%"},{b:"%[qQwWx]?-",e:"-"},{b:"%[qQwWx]?\\|",e:"\\|"},{b:/\B\?(\\\d{1,3}|\\x[A-Fa-f0-9]{1,2}|\\u[A-Fa-f0-9]{4}|\\?\S)\b/},{b:/<<(-?)\w+$/,e:/^\s*\w+$/}]},i={cN:"params",b:"\\(",e:"\\)",endsParent:!0,k:r},d=[t,a,{cN:"class",bK:"class module",e:"$|;",i:/=/,c:[e.inherit(e.TM,{b:"[A-Za-z_]\\w*(::\\w+)*(\\?|\\!)?"}),{b:"<\\s*",c:[{b:"("+e.IR+"::)?"+e.IR}]}].concat(s)},{cN:"function",bK:"def",e:"$|;",c:[e.inherit(e.TM,{b:b}),i].concat(s)},{b:e.IR+"::"},{cN:"symbol",b:e.UIR+"(\\!|\\?)?:",r:0},{cN:"symbol",b:":(?!\\s)",c:[t,{b:b}],r:0},{cN:"number",b:"(\\b0[0-7_]+)|(\\b0x[0-9a-fA-F_]+)|(\\b[1-9][0-9_]*(\\.[0-9_]+)?)|[0_]\\b",r:0},{b:"(\\$\\W)|((\\$|\\@\\@?)(\\w+))"},{cN:"params",b:/\|/,e:/\|/,k:r},{b:"("+e.RSR+"|unless)\\s*",k:"unless",c:[a,{cN:"regexp",c:[e.BE,n],i:/\n/,v:[{b:"/",e:"/[a-z]*"},{b:"%r{",e:"}[a-z]*"},{b:"%r\\(",e:"\\)[a-z]*"},{b:"%r!",e:"![a-z]*"},{b:"%r\\[",e:"\\][a-z]*"}]}].concat(s),r:0}].concat(s);n.c=d,i.c=d;var l="[>?]>",o="[\\w#]+\\(\\w+\\):\\d+:\\d+>",u="(\\w+-)?\\d+\\.\\d+\\.\\d(p\\d+)?[^>]+>",w=[{b:/^\s*=>/,starts:{e:"$",c:d}},{cN:"meta",b:"^("+l+"|"+o+"|"+u+")",starts:{e:"$",c:d}}];return{aliases:["rb","gemspec","podspec","thor","irb"],k:r,i:/\/\*/,c:s.concat(w).concat(d)}});hljs.registerLanguage("javascript",function(e){var r="[A-Za-z$_][0-9A-Za-z$_]*",t={keyword:"in of if for while finally var new function do return void else break catch instanceof with throw case default try this switch continue typeof delete let yield const export super debugger as async await static import from as",literal:"true false null undefined NaN Infinity",built_in:"eval isFinite isNaN parseFloat parseInt decodeURI decodeURIComponent encodeURI encodeURIComponent escape unescape Object Function Boolean Error EvalError InternalError RangeError ReferenceError StopIteration SyntaxError TypeError URIError Number Math Date String RegExp Array Float32Array Float64Array Int16Array Int32Array Int8Array Uint16Array Uint32Array Uint8Array Uint8ClampedArray ArrayBuffer DataView JSON Intl arguments require module console window document Symbol Set Map WeakSet WeakMap Proxy Reflect Promise"},a={cN:"number",v:[{b:"\\b(0[bB][01]+)"},{b:"\\b(0[oO][0-7]+)"},{b:e.CNR}],r:0},n={cN:"subst",b:"\\$\\{",e:"\\}",k:t,c:[]},c={cN:"string",b:"`",e:"`",c:[e.BE,n]};n.c=[e.ASM,e.QSM,c,a,e.RM];var s=n.c.concat([e.CBCM,e.CLCM]);return{aliases:["js","jsx"],k:t,c:[{cN:"meta",r:10,b:/^\s*['"]use (strict|asm)['"]/},{cN:"meta",b:/^#!/,e:/$/},e.ASM,e.QSM,c,e.CLCM,e.CBCM,a,{b:/[{,]\s*/,r:0,c:[{b:r+"\\s*:",rB:!0,r:0,c:[{cN:"attr",b:r,r:0}]}]},{b:"("+e.RSR+"|\\b(case|return|throw)\\b)\\s*",k:"return throw case",c:[e.CLCM,e.CBCM,e.RM,{cN:"function",b:"(\\(.*?\\)|"+r+")\\s*=>",rB:!0,e:"\\s*=>",c:[{cN:"params",v:[{b:r},{b:/\(\s*\)/},{b:/\(/,e:/\)/,eB:!0,eE:!0,k:t,c:s}]}]},{b:/</,e:/(\/\w+|\w+\/)>/,sL:"xml",c:[{b:/<\w+\s*\/>/,skip:!0},{b:/<\w+/,e:/(\/\w+|\w+\/)>/,skip:!0,c:[{b:/<\w+\s*\/>/,skip:!0},"self"]}]}],r:0},{cN:"function",bK:"function",e:/\{/,eE:!0,c:[e.inherit(e.TM,{b:r}),{cN:"params",b:/\(/,e:/\)/,eB:!0,eE:!0,c:s}],i:/\[|%/},{b:/\$[(.]/},e.METHOD_GUARD,{cN:"class",bK:"class",e:/[{;=]/,eE:!0,i:/[:"\[\]]/,c:[{bK:"extends"},e.UTM]},{bK:"constructor",e:/\{/,eE:!0}],i:/#(?!!)/}});hljs.registerLanguage("cpp",function(t){var e={cN:"keyword",b:"\\b[a-z\\d_]*_t\\b"},r={cN:"string",v:[{b:'(u8?|U)?L?"',e:'"',i:"\\n",c:[t.BE]},{b:'(u8?|U)?R"',e:'"',c:[t.BE]},{b:"'\\\\?.",e:"'",i:"."}]},s={cN:"number",v:[{b:"\\b(0b[01']+)"},{b:"(-?)\\b([\\d']+(\\.[\\d']*)?|\\.[\\d']+)(u|U|l|L|ul|UL|f|F|b|B)"},{b:"(-?)(\\b0[xX][a-fA-F0-9']+|(\\b[\\d']+(\\.[\\d']*)?|\\.[\\d']+)([eE][-+]?[\\d']+)?)"}],r:0},i={cN:"meta",b:/#\s*[a-z]+\b/,e:/$/,k:{"meta-keyword":"if else elif endif define undef warning error line pragma ifdef ifndef include"},c:[{b:/\\\n/,r:0},t.inherit(r,{cN:"meta-string"}),{cN:"meta-string",b:/<[^\n>]*>/,e:/$/,i:"\\n"},t.CLCM,t.CBCM]},a=t.IR+"\\s*\\(",c={keyword:"int float while private char catch import module export virtual operator sizeof dynamic_cast|10 typedef const_cast|10 const for static_cast|10 union namespace unsigned long volatile static protected bool template mutable if public friend do goto auto void enum else break extern using asm case typeid short reinterpret_cast|10 default double register explicit signed typename try this switch continue inline delete alignof constexpr decltype noexcept static_assert thread_local restrict _Bool complex _Complex _Imaginary atomic_bool atomic_char atomic_schar atomic_uchar atomic_short atomic_ushort atomic_int atomic_uint atomic_long atomic_ulong atomic_llong atomic_ullong new throw return and or not",built_in:"std string cin cout cerr clog stdin stdout stderr stringstream istringstream ostringstream auto_ptr deque list queue stack vector map set bitset multiset multimap unordered_set unordered_map unordered_multiset unordered_multimap array shared_ptr abort abs acos asin atan2 atan calloc ceil cosh cos exit exp fabs floor fmod fprintf fputs free frexp fscanf isalnum isalpha iscntrl isdigit isgraph islower isprint ispunct isspace isupper isxdigit tolower toupper labs ldexp log10 log malloc realloc memchr memcmp memcpy memset modf pow printf putchar puts scanf sinh sin snprintf sprintf sqrt sscanf strcat strchr strcmp strcpy strcspn strlen strncat strncmp strncpy strpbrk strrchr strspn strstr tanh tan vfprintf vprintf vsprintf endl initializer_list unique_ptr",literal:"true false nullptr NULL"},n=[e,t.CLCM,t.CBCM,s,r];return{aliases:["c","cc","h","c++","h++","hpp"],k:c,i:"</",c:n.concat([i,{b:"\\b(deque|list|queue|stack|vector|map|set|bitset|multiset|multimap|unordered_map|unordered_set|unordered_multiset|unordered_multimap|array)\\s*<",e:">",k:c,c:["self",e]},{b:t.IR+"::",k:c},{v:[{b:/=/,e:/;/},{b:/\(/,e:/\)/},{bK:"new throw return else",e:/;/}],k:c,c:n.concat([{b:/\(/,e:/\)/,k:c,c:n.concat(["self"]),r:0}]),r:0},{cN:"function",b:"("+t.IR+"[\\*&\\s]+)+"+a,rB:!0,e:/[{;=]/,eE:!0,k:c,i:/[^\w\s\*&]/,c:[{b:a,rB:!0,c:[t.TM],r:0},{cN:"params",b:/\(/,e:/\)/,k:c,r:0,c:[t.CLCM,t.CBCM,r,s,e]},t.CLCM,t.CBCM,i]},{cN:"class",bK:"class struct",e:/[{;:]/,c:[{b:/</,e:/>/,c:["self"]},t.TM]}]),exports:{preprocessor:i,strings:r,k:c}}});hljs.registerLanguage("python",function(e){var r={keyword:"and elif is global as in if from raise for except finally print import pass return exec else break not with class assert yield try while continue del or def lambda async await nonlocal|10 None True False",built_in:"Ellipsis NotImplemented"},b={cN:"meta",b:/^(>>>|\.\.\.) /},c={cN:"subst",b:/\{/,e:/\}/,k:r,i:/#/},a={cN:"string",c:[e.BE],v:[{b:/(u|b)?r?'''/,e:/'''/,c:[b],r:10},{b:/(u|b)?r?"""/,e:/"""/,c:[b],r:10},{b:/(fr|rf|f)'''/,e:/'''/,c:[b,c]},{b:/(fr|rf|f)"""/,e:/"""/,c:[b,c]},{b:/(u|r|ur)'/,e:/'/,r:10},{b:/(u|r|ur)"/,e:/"/,r:10},{b:/(b|br)'/,e:/'/},{b:/(b|br)"/,e:/"/},{b:/(fr|rf|f)'/,e:/'/,c:[c]},{b:/(fr|rf|f)"/,e:/"/,c:[c]},e.ASM,e.QSM]},s={cN:"number",r:0,v:[{b:e.BNR+"[lLjJ]?"},{b:"\\b(0o[0-7]+)[lLjJ]?"},{b:e.CNR+"[lLjJ]?"}]},i={cN:"params",b:/\(/,e:/\)/,c:["self",b,s,a]};return c.c=[a,s,b],{aliases:["py","gyp"],k:r,i:/(<\/|->|\?)|=>/,c:[b,s,a,e.HCM,{v:[{cN:"function",bK:"def"},{cN:"class",bK:"class"}],e:/:/,i:/[${=;\n,]/,c:[e.UTM,i,{b:/->/,eW:!0,k:"None"}]},{cN:"meta",b:/^[\t ]*@/,e:/$/},{b:/\b(print|exec)\(/}]}});hljs.registerLanguage("shell",function(s){return{aliases:["console"],c:[{cN:"meta",b:"^\\s{0,3}[\\w\\d\\[\\]()@-]*[>%$#]",starts:{e:"$",sL:"bash"}}]}});hljs.registerLanguage("ini",function(e){var b={cN:"string",c:[e.BE],v:[{b:"'''",e:"'''",r:10},{b:'"""',e:'"""',r:10},{b:'"',e:'"'},{b:"'",e:"'"}]};return{aliases:["toml"],cI:!0,i:/\S/,c:[e.C(";","$"),e.HCM,{cN:"section",b:/^\s*\[+/,e:/\]+/},{b:/^[a-z0-9\[\]_-]+\s*=\s*/,e:"$",rB:!0,c:[{cN:"attr",b:/[a-z0-9\[\]_-]+/},{b:/=/,eW:!0,r:0,c:[{cN:"literal",b:/\bon|off|true|false|yes|no\b/},{cN:"variable",v:[{b:/\$[\w\d"][\w\d_]*/},{b:/\$\{(.*?)}/}]},b,{cN:"number",b:/([\+\-]+)?[\d]+_[\d_]+/},e.NM]}]}]}});hljs.registerLanguage("java",function(e){var a="[À-ʸa-zA-Z_$][À-ʸa-zA-Z_$0-9]*",t=a+"(<"+a+"(\\s*,\\s*"+a+")*>)?",r="false synchronized int abstract float private char boolean static null if const for true while long strictfp finally protected import native final void enum else break transient catch instanceof byte super volatile case assert short package default double public try this switch continue throws protected public private module requires exports do",s="\\b(0[bB]([01]+[01_]+[01]+|[01]+)|0[xX]([a-fA-F0-9]+[a-fA-F0-9_]+[a-fA-F0-9]+|[a-fA-F0-9]+)|(([\\d]+[\\d_]+[\\d]+|[\\d]+)(\\.([\\d]+[\\d_]+[\\d]+|[\\d]+))?|\\.([\\d]+[\\d_]+[\\d]+|[\\d]+))([eE][-+]?\\d+)?)[lLfF]?",c={cN:"number",b:s,r:0};return{aliases:["jsp"],k:r,i:/<\/|#/,c:[e.C("/\\*\\*","\\*/",{r:0,c:[{b:/\w+@/,r:0},{cN:"doctag",b:"@[A-Za-z]+"}]}),e.CLCM,e.CBCM,e.ASM,e.QSM,{cN:"class",bK:"class interface",e:/[{;=]/,eE:!0,k:"class interface",i:/[:"\[\]]/,c:[{bK:"extends implements"},e.UTM]},{bK:"new throw return else",r:0},{cN:"function",b:"("+t+"\\s+)+"+e.UIR+"\\s*\\(",rB:!0,e:/[{;=]/,eE:!0,k:r,c:[{b:e.UIR+"\\s*\\(",rB:!0,r:0,c:[e.UTM]},{cN:"params",b:/\(/,e:/\)/,k:r,r:0,c:[e.ASM,e.QSM,e.CNM,e.CBCM]},e.CLCM,e.CBCM]},c,{cN:"meta",b:"@[A-Za-z]+"}]}});hljs.registerLanguage("cs",function(e){var i={keyword:"abstract as base bool break byte case catch char checked const continue decimal default delegate do double else enum event explicit extern finally fixed float for foreach goto if implicit in int interface internal is lock long object operator out override params private protected public readonly ref sbyte sealed short sizeof stackalloc static string struct switch this try typeof uint ulong unchecked unsafe ushort using virtual void volatile while nameof add alias ascending async await by descending dynamic equals from get global group into join let on orderby partial remove select set value var where yield",literal:"null false true"},r={cN:"string",b:'@"',e:'"',c:[{b:'""'}]},t=e.inherit(r,{i:/\n/}),a={cN:"subst",b:"{",e:"}",k:i},n=e.inherit(a,{i:/\n/}),c={cN:"string",b:/\$"/,e:'"',i:/\n/,c:[{b:"{{"},{b:"}}"},e.BE,n]},s={cN:"string",b:/\$@"/,e:'"',c:[{b:"{{"},{b:"}}"},{b:'""'},a]},o=e.inherit(s,{i:/\n/,c:[{b:"{{"},{b:"}}"},{b:'""'},n]});a.c=[s,c,r,e.ASM,e.QSM,e.CNM,e.CBCM],n.c=[o,c,t,e.ASM,e.QSM,e.CNM,e.inherit(e.CBCM,{i:/\n/})];var l={v:[s,c,r,e.ASM,e.QSM]},b=e.IR+"(<"+e.IR+"(\\s*,\\s*"+e.IR+")*>)?(\\[\\])?";return{aliases:["csharp"],k:i,i:/::/,c:[e.C("///","$",{rB:!0,c:[{cN:"doctag",v:[{b:"///",r:0},{b:"<!--|-->"},{b:"</?",e:">"}]}]}),e.CLCM,e.CBCM,{cN:"meta",b:"#",e:"$",k:{"meta-keyword":"if else elif endif define undef warning error line region endregion pragma checksum"}},l,e.CNM,{bK:"class interface",e:/[{;=]/,i:/[^\s:]/,c:[e.TM,e.CLCM,e.CBCM]},{bK:"namespace",e:/[{;=]/,i:/[^\s:]/,c:[e.inherit(e.TM,{b:"[a-zA-Z](\\.?\\w)*"}),e.CLCM,e.CBCM]},{bK:"new return throw await",r:0},{cN:"function",b:"("+b+"\\s+)+"+e.IR+"\\s*\\(",rB:!0,e:/[{;=]/,eE:!0,k:i,c:[{b:e.IR+"\\s*\\(",rB:!0,c:[e.TM],r:0},{cN:"params",b:/\(/,e:/\)/,eB:!0,eE:!0,k:i,r:0,c:[l,e.CNM,e.CBCM]},e.CLCM,e.CBCM]}]}});hljs.registerLanguage("sql",function(e){var t=e.C("--","$");return{cI:!0,i:/[<>{}*#]/,c:[{bK:"begin end start commit rollback savepoint lock alter create drop rename call delete do handler insert load replace select truncate update set show pragma grant merge describe use explain help declare prepare execute deallocate release unlock purge reset change stop analyze cache flush optimize repair kill install uninstall checksum restore check backup revoke comment",e:/;/,eW:!0,l:/[\w\.]+/,k:{keyword:"abort abs absolute acc acce accep accept access accessed accessible account acos action activate add addtime admin administer advanced advise aes_decrypt aes_encrypt after agent aggregate ali alia alias allocate allow alter always analyze ancillary and any anydata anydataset anyschema anytype apply archive archived archivelog are as asc ascii asin assembly assertion associate asynchronous at atan atn2 attr attri attrib attribu attribut attribute attributes audit authenticated authentication authid authors auto autoallocate autodblink autoextend automatic availability avg backup badfile basicfile before begin beginning benchmark between bfile bfile_base big bigfile bin binary_double binary_float binlog bit_and bit_count bit_length bit_or bit_xor bitmap blob_base block blocksize body both bound buffer_cache buffer_pool build bulk by byte byteordermark bytes cache caching call calling cancel capacity cascade cascaded case cast catalog category ceil ceiling chain change changed char_base char_length character_length characters characterset charindex charset charsetform charsetid check checksum checksum_agg child choose chr chunk class cleanup clear client clob clob_base clone close cluster_id cluster_probability cluster_set clustering coalesce coercibility col collate collation collect colu colum column column_value columns columns_updated comment commit compact compatibility compiled complete composite_limit compound compress compute concat concat_ws concurrent confirm conn connec connect connect_by_iscycle connect_by_isleaf connect_by_root connect_time connection consider consistent constant constraint constraints constructor container content contents context contributors controlfile conv convert convert_tz corr corr_k corr_s corresponding corruption cos cost count count_big counted covar_pop covar_samp cpu_per_call cpu_per_session crc32 create creation critical cross cube cume_dist curdate current current_date current_time current_timestamp current_user cursor curtime customdatum cycle data database databases datafile datafiles datalength date_add date_cache date_format date_sub dateadd datediff datefromparts datename datepart datetime2fromparts day day_to_second dayname dayofmonth dayofweek dayofyear days db_role_change dbtimezone ddl deallocate declare decode decompose decrement decrypt deduplicate def defa defau defaul default defaults deferred defi defin define degrees delayed delegate delete delete_all delimited demand dense_rank depth dequeue des_decrypt des_encrypt des_key_file desc descr descri describ describe descriptor deterministic diagnostics difference dimension direct_load directory disable disable_all disallow disassociate discardfile disconnect diskgroup distinct distinctrow distribute distributed div do document domain dotnet double downgrade drop dumpfile duplicate duration each edition editionable editions element ellipsis else elsif elt empty enable enable_all enclosed encode encoding encrypt end end-exec endian enforced engine engines enqueue enterprise entityescaping eomonth error errors escaped evalname evaluate event eventdata events except exception exceptions exchange exclude excluding execu execut execute exempt exists exit exp expire explain export export_set extended extent external external_1 external_2 externally extract failed failed_login_attempts failover failure far fast feature_set feature_value fetch field fields file file_name_convert filesystem_like_logging final finish first first_value fixed flash_cache flashback floor flush following follows for forall force form forma format found found_rows freelist freelists freepools fresh from from_base64 from_days ftp full function general generated get get_format get_lock getdate getutcdate global global_name globally go goto grant grants greatest group group_concat group_id grouping grouping_id groups gtid_subtract guarantee guard handler hash hashkeys having hea head headi headin heading heap help hex hierarchy high high_priority hosts hour http id ident_current ident_incr ident_seed identified identity idle_time if ifnull ignore iif ilike ilm immediate import in include including increment index indexes indexing indextype indicator indices inet6_aton inet6_ntoa inet_aton inet_ntoa infile initial initialized initially initrans inmemory inner innodb input insert install instance instantiable instr interface interleaved intersect into invalidate invisible is is_free_lock is_ipv4 is_ipv4_compat is_not is_not_null is_used_lock isdate isnull isolation iterate java join json json_exists keep keep_duplicates key keys kill language large last last_day last_insert_id last_value lax lcase lead leading least leaves left len lenght length less level levels library like like2 like4 likec limit lines link list listagg little ln load load_file lob lobs local localtime localtimestamp locate locator lock locked log log10 log2 logfile logfiles logging logical logical_reads_per_call logoff logon logs long loop low low_priority lower lpad lrtrim ltrim main make_set makedate maketime managed management manual map mapping mask master master_pos_wait match matched materialized max maxextents maximize maxinstances maxlen maxlogfiles maxloghistory maxlogmembers maxsize maxtrans md5 measures median medium member memcompress memory merge microsecond mid migration min minextents minimum mining minus minute minvalue missing mod mode model modification modify module monitoring month months mount move movement multiset mutex name name_const names nan national native natural nav nchar nclob nested never new newline next nextval no no_write_to_binlog noarchivelog noaudit nobadfile nocheck nocompress nocopy nocycle nodelay nodiscardfile noentityescaping noguarantee nokeep nologfile nomapping nomaxvalue nominimize nominvalue nomonitoring none noneditionable nonschema noorder nopr nopro noprom nopromp noprompt norely noresetlogs noreverse normal norowdependencies noschemacheck noswitch not nothing notice notrim novalidate now nowait nth_value nullif nulls num numb numbe nvarchar nvarchar2 object ocicoll ocidate ocidatetime ociduration ociinterval ociloblocator ocinumber ociref ocirefcursor ocirowid ocistring ocitype oct octet_length of off offline offset oid oidindex old on online only opaque open operations operator optimal optimize option optionally or oracle oracle_date oradata ord ordaudio orddicom orddoc order ordimage ordinality ordvideo organization orlany orlvary out outer outfile outline output over overflow overriding package pad parallel parallel_enable parameters parent parse partial partition partitions pascal passing password password_grace_time password_lock_time password_reuse_max password_reuse_time password_verify_function patch path patindex pctincrease pctthreshold pctused pctversion percent percent_rank percentile_cont percentile_disc performance period period_add period_diff permanent physical pi pipe pipelined pivot pluggable plugin policy position post_transaction pow power pragma prebuilt precedes preceding precision prediction prediction_cost prediction_details prediction_probability prediction_set prepare present preserve prior priority private private_sga privileges procedural procedure procedure_analyze processlist profiles project prompt protection public publishingservername purge quarter query quick quiesce quota quotename radians raise rand range rank raw read reads readsize rebuild record records recover recovery recursive recycle redo reduced ref reference referenced references referencing refresh regexp_like register regr_avgx regr_avgy regr_count regr_intercept regr_r2 regr_slope regr_sxx regr_sxy reject rekey relational relative relaylog release release_lock relies_on relocate rely rem remainder rename repair repeat replace replicate replication required reset resetlogs resize resource respect restore restricted result result_cache resumable resume retention return returning returns reuse reverse revoke right rlike role roles rollback rolling rollup round row row_count rowdependencies rowid rownum rows rtrim rules safe salt sample save savepoint sb1 sb2 sb4 scan schema schemacheck scn scope scroll sdo_georaster sdo_topo_geometry search sec_to_time second section securefile security seed segment select self sequence sequential serializable server servererror session session_user sessions_per_user set sets settings sha sha1 sha2 share shared shared_pool short show shrink shutdown si_averagecolor si_colorhistogram si_featurelist si_positionalcolor si_stillimage si_texture siblings sid sign sin size size_t sizes skip slave sleep smalldatetimefromparts smallfile snapshot some soname sort soundex source space sparse spfile split sql sql_big_result sql_buffer_result sql_cache sql_calc_found_rows sql_small_result sql_variant_property sqlcode sqldata sqlerror sqlname sqlstate sqrt square standalone standby start starting startup statement static statistics stats_binomial_test stats_crosstab stats_ks_test stats_mode stats_mw_test stats_one_way_anova stats_t_test_ stats_t_test_indep stats_t_test_one stats_t_test_paired stats_wsr_test status std stddev stddev_pop stddev_samp stdev stop storage store stored str str_to_date straight_join strcmp strict string struct stuff style subdate subpartition subpartitions substitutable substr substring subtime subtring_index subtype success sum suspend switch switchoffset switchover sync synchronous synonym sys sys_xmlagg sysasm sysaux sysdate sysdatetimeoffset sysdba sysoper system system_user sysutcdatetime table tables tablespace tan tdo template temporary terminated tertiary_weights test than then thread through tier ties time time_format time_zone timediff timefromparts timeout timestamp timestampadd timestampdiff timezone_abbr timezone_minute timezone_region to to_base64 to_date to_days to_seconds todatetimeoffset trace tracking transaction transactional translate translation treat trigger trigger_nestlevel triggers trim truncate try_cast try_convert try_parse type ub1 ub2 ub4 ucase unarchived unbounded uncompress under undo unhex unicode uniform uninstall union unique unix_timestamp unknown unlimited unlock unpivot unrecoverable unsafe unsigned until untrusted unusable unused update updated upgrade upped upper upsert url urowid usable usage use use_stored_outlines user user_data user_resources users using utc_date utc_timestamp uuid uuid_short validate validate_password_strength validation valist value values var var_samp varcharc vari varia variab variabl variable variables variance varp varraw varrawc varray verify version versions view virtual visible void wait wallet warning warnings week weekday weekofyear wellformed when whene whenev wheneve whenever where while whitespace with within without work wrapped xdb xml xmlagg xmlattributes xmlcast xmlcolattval xmlelement xmlexists xmlforest xmlindex xmlnamespaces xmlpi xmlquery xmlroot xmlschema xmlserialize xmltable xmltype xor year year_to_month years yearweek",literal:"true false null",built_in:"array bigint binary bit blob boolean char character date dec decimal float int int8 integer interval number numeric real record serial serial8 smallint text varchar varying void"},c:[{cN:"string",b:"'",e:"'",c:[e.BE,{b:"''"}]},{cN:"string",b:'"',e:'"',c:[e.BE,{b:'""'}]},{cN:"string",b:"`",e:"`",c:[e.BE]},e.CNM,e.CBCM,t]},e.CBCM,t]}});hljs.registerLanguage("objectivec",function(e){var t={cN:"built_in",b:"\\b(AV|CA|CF|CG|CI|CL|CM|CN|CT|MK|MP|MTK|MTL|NS|SCN|SK|UI|WK|XC)\\w+"},_={keyword:"int float while char export sizeof typedef const struct for union unsigned long volatile static bool mutable if do return goto void enum else break extern asm case short default double register explicit signed typename this switch continue wchar_t inline readonly assign readwrite self @synchronized id typeof nonatomic super unichar IBOutlet IBAction strong weak copy in out inout bycopy byref oneway __strong __weak __block __autoreleasing @private @protected @public @try @property @end @throw @catch @finally @autoreleasepool @synthesize @dynamic @selector @optional @required @encode @package @import @defs @compatibility_alias __bridge __bridge_transfer __bridge_retained __bridge_retain __covariant __contravariant __kindof _Nonnull _Nullable _Null_unspecified __FUNCTION__ __PRETTY_FUNCTION__ __attribute__ getter setter retain unsafe_unretained nonnull nullable null_unspecified null_resettable class instancetype NS_DESIGNATED_INITIALIZER NS_UNAVAILABLE NS_REQUIRES_SUPER NS_RETURNS_INNER_POINTER NS_INLINE NS_AVAILABLE NS_DEPRECATED NS_ENUM NS_OPTIONS NS_SWIFT_UNAVAILABLE NS_ASSUME_NONNULL_BEGIN NS_ASSUME_NONNULL_END NS_REFINED_FOR_SWIFT NS_SWIFT_NAME NS_SWIFT_NOTHROW NS_DURING NS_HANDLER NS_ENDHANDLER NS_VALUERETURN NS_VOIDRETURN",literal:"false true FALSE TRUE nil YES NO NULL",built_in:"BOOL dispatch_once_t dispatch_queue_t dispatch_sync dispatch_async dispatch_once"},i=/[a-zA-Z@][a-zA-Z0-9_]*/,n="@interface @class @protocol @implementation";return{aliases:["mm","objc","obj-c"],k:_,l:i,i:"</",c:[t,e.CLCM,e.CBCM,e.CNM,e.QSM,{cN:"string",v:[{b:'@"',e:'"',i:"\\n",c:[e.BE]},{b:"'",e:"[^\\\\]'",i:"[^\\\\][^']"}]},{cN:"meta",b:"#",e:"$",c:[{cN:"meta-string",v:[{b:'"',e:'"'},{b:"<",e:">"}]}]},{cN:"class",b:"("+n.split(" ").join("|")+")\\b",e:"({|$)",eE:!0,k:n,l:i,c:[e.UTM]},{b:"\\."+e.UIR,r:0}]}});