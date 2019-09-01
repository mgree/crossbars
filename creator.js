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



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
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

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
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

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
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

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
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
	if (region.aE.X === region.aQ.X)
	{
		return 'on line ' + region.aE.X;
	}
	return 'on lines ' + region.aE.X + ' through ' + region.aQ.X;
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

	/**_UNUSED/
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

	/**/
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

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
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

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


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



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


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
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
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



/**_UNUSED/
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

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
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
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
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

		case 9:
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

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
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

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
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

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
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

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

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
		impl.bU,
		impl.cn,
		impl.cg,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
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


function _Platform_export(exports)
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


function _Platform_export_UNUSED(exports)
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



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.bJ.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done(elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done(elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.bJ.b, xhr)); });
		elm$core$Maybe$isJust(request.cm) && _Http_track(router, xhr, request.cm.a);

		try {
			xhr.open(request.bW, request.S, true);
		} catch (e) {
			return done(elm$http$Http$BadUrl_(request.S));
		}

		_Http_configureRequest(xhr, request);

		request.bB.a && xhr.setRequestHeader('Content-Type', request.bB.a);
		xhr.send(request.bB.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.bR; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.cl.a || 0;
	xhr.responseType = request.bJ.d;
	xhr.withCredentials = request.K;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? elm$http$Http$GoodStatus_ : elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		S: xhr.responseURL,
		bq: xhr.status,
		cf: xhr.statusText,
		bR: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return elm$core$Dict$empty;
	}

	var headers = elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
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


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Sending({
			cc: event.loaded,
			aD: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2(elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, elm$http$Http$Receiving({
			b9: event.loaded,
			aD: event.lengthComputable ? elm$core$Maybe$Just(event.total) : elm$core$Maybe$Nothing
		}))));
	});
}


function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2(elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});




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

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
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

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
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
		y: func(record.y),
		aF: record.aF,
		aB: record.aB
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
		var message = !tag ? value : tag < 3 ? value.a : value.y;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.aF;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.aB) && event.preventDefault(),
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




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.bU,
		impl.cn,
		impl.cg,
		function(sendToApp, initialModel) {
			var view = impl.co;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
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
		impl.bU,
		impl.cn,
		impl.cg,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.ab && impl.ab(sendToApp)
			var view = impl.co;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.bB);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bv) && (_VirtualDom_doc.title = title = doc.bv);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


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
	var onUrlChange = impl.b5;
	var onUrlRequest = impl.b6;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		ab: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bd === next.bd
							&& curr.aZ === next.aZ
							&& curr.a9.a === next.a9.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		bU: function(flags)
		{
			return A3(impl.bU, flags, _Browser_getUrl(), key);
		},
		co: impl.co,
		cn: impl.cn,
		cg: impl.cg
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
		? { bS: 'hidden', bD: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { bS: 'mozHidden', bD: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { bS: 'msHidden', bD: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { bS: 'webkitHidden', bD: 'webkitvisibilitychange' }
		: { bS: 'hidden', bD: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
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
		bm: _Browser_getScene(),
		bx: {
			as: _Browser_window.pageXOffset,
			at: _Browser_window.pageYOffset,
			U: _Browser_doc.documentElement.clientWidth,
			N: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		U: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		N: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
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
			bm: {
				U: node.scrollWidth,
				N: node.scrollHeight
			},
			bx: {
				as: node.scrollLeft,
				at: node.scrollTop,
				U: node.clientWidth,
				N: node.clientHeight
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
			bm: _Browser_getScene(),
			bx: {
				as: x,
				at: y,
				U: _Browser_doc.documentElement.clientWidth,
				N: _Browser_doc.documentElement.clientHeight
			},
			bH: {
				as: x + rect.left,
				at: y + rect.top,
				U: rect.width,
				N: rect.height
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
var author$project$Main$GotWordlist = F2(
	function (a, b) {
		return {$: 20, a: a, b: b};
	});
var author$project$Main$TimeZone = function (a) {
	return {$: 19, a: a};
};
var author$project$Puzzle$QuoteEntry = 0;
var elm$core$Basics$EQ = 1;
var elm$core$Basics$LT = 0;
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
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
var elm$core$List$cons = _List_cons;
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$GT = 2;
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
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
	var dict = _n0;
	return elm$core$Dict$keys(dict);
};
var elm$core$Basics$identity = function (x) {
	return x;
};
var elm$time$Time$Posix = elm$core$Basics$identity;
var elm$time$Time$millisToPosix = elm$core$Basics$identity;
var author$project$Puzzle$emptyPuzzle = {
	aJ: '',
	W: _List_Nil,
	H: 0,
	aC: '',
	B: elm$time$Time$millisToPosix(0),
	bv: ''
};
var author$project$SMT$SolverUnloaded = 0;
var elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var author$project$Wordlist$emptyTrie = elm$core$Dict$empty;
var author$project$Wordlist$empty = author$project$Wordlist$emptyTrie;
var elm$core$Basics$False = 1;
var elm$core$Maybe$Nothing = {$: 1};
var elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$time$Time$utc = A2(elm$time$Time$Zone, 0, _List_Nil);
var author$project$Main$emptyModel = {_: false, A: elm$core$Dict$empty, e: author$project$Puzzle$emptyPuzzle, s: _List_Nil, t: _List_Nil, I: elm$core$Maybe$Nothing, an: elm$core$Maybe$Nothing, ao: 0, aq: elm$time$Time$utc, ar: author$project$Wordlist$empty};
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$append = _Utils_append;
var elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var elm$core$String$uncons = _String_uncons;
var elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
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
	return {$: 1, a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
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
var elm$core$Basics$eq = _Utils_equal;
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
		if (!builder.g) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.i),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.i);
		} else {
			var treeLen = builder.g * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.j) : builder.j;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.g);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.i) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.i);
		}
	});
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
					{j: nodeList, g: (len / elm$core$Array$branchFactor) | 0, i: tail});
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
var elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var elm$core$Basics$True = 0;
var elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
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
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 1) {
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
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
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
var elm$json$Json$Decode$andThen = _Json_andThen;
var elm$json$Json$Decode$fail = _Json_fail;
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$int = _Json_decodeInt;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$null = _Json_decodeNull;
var elm$json$Json$Decode$oneOf = _Json_oneOf;
var elm$json$Json$Decode$nullable = function (decoder) {
	return elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				elm$json$Json$Decode$null(elm$core$Maybe$Nothing),
				A2(elm$json$Json$Decode$map, elm$core$Maybe$Just, decoder)
			]));
};
var elm$json$Json$Decode$string = _Json_decodeString;
var elm$json$Json$Decode$succeed = _Json_succeed;
var author$project$Puzzle$decodeAnswer = A3(
	elm$json$Json$Decode$map2,
	elm$core$Tuple$pair,
	A2(
		elm$json$Json$Decode$field,
		'number',
		elm$json$Json$Decode$nullable(elm$json$Json$Decode$int)),
	A2(
		elm$json$Json$Decode$field,
		'char',
		A2(
			elm$json$Json$Decode$andThen,
			function (s) {
				var _n0 = elm$core$String$uncons(s);
				if ((!_n0.$) && (_n0.a.b === '')) {
					var _n1 = _n0.a;
					var c = _n1.a;
					return elm$json$Json$Decode$succeed(c);
				} else {
					return elm$json$Json$Decode$fail('expected single character in answer, found \'' + (s + '\''));
				}
			},
			elm$json$Json$Decode$string)));
var elm$json$Json$Decode$list = _Json_decodeList;
var elm$json$Json$Decode$map3 = _Json_map3;
var author$project$Puzzle$decodeClue = A4(
	elm$json$Json$Decode$map3,
	F3(
		function (hint, text, answer) {
			return {ag: answer, aY: hint, cj: text};
		}),
	A2(elm$json$Json$Decode$field, 'hint', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'text', elm$json$Json$Decode$string),
	A2(
		elm$json$Json$Decode$field,
		'answer',
		elm$json$Json$Decode$list(author$project$Puzzle$decodeAnswer)));
var author$project$Puzzle$Anagramming = 1;
var author$project$Puzzle$CluingLettering = 2;
var author$project$Puzzle$decodePhase = A2(
	elm$json$Json$Decode$andThen,
	function (s) {
		switch (s) {
			case 'QuoteEntry':
				return elm$json$Json$Decode$succeed(0);
			case 'Anagramming':
				return elm$json$Json$Decode$succeed(1);
			case 'CluingLettering':
				return elm$json$Json$Decode$succeed(2);
			default:
				return elm$json$Json$Decode$fail('invalid phase \'' + (s + '\''));
		}
	},
	elm$json$Json$Decode$string);
var elm$json$Json$Decode$map6 = _Json_map6;
var author$project$Puzzle$decodePuzzle = A7(
	elm$json$Json$Decode$map6,
	F6(
		function (title, author, quote, clues, phase, timeModified) {
			return {aJ: author, W: clues, H: phase, aC: quote, B: timeModified, bv: title};
		}),
	A2(elm$json$Json$Decode$field, 'title', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'author', elm$json$Json$Decode$string),
	A2(elm$json$Json$Decode$field, 'quote', elm$json$Json$Decode$string),
	A2(
		elm$json$Json$Decode$field,
		'clues',
		elm$json$Json$Decode$list(author$project$Puzzle$decodeClue)),
	A2(elm$json$Json$Decode$field, 'phase', author$project$Puzzle$decodePhase),
	A2(
		elm$json$Json$Decode$field,
		'timeModified',
		A2(elm$json$Json$Decode$map, elm$time$Time$millisToPosix, elm$json$Json$Decode$int)));
var author$project$Main$decodeModel = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (puzzle, savedPuzzles) {
			return _Utils_update(
				author$project$Main$emptyModel,
				{e: puzzle, s: savedPuzzles});
		}),
	A2(elm$json$Json$Decode$field, 'currentPuzzle', author$project$Puzzle$decodePuzzle),
	A2(
		elm$json$Json$Decode$field,
		'savedPuzzles',
		elm$json$Json$Decode$list(author$project$Puzzle$decodePuzzle)));
var author$project$Main$wordlists = _List_fromArray(
	[
		{ap: 'FreeBSD words list', S: 'words/words.txt'}
	]);
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
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var elm$core$Task$Perform = elm$core$Basics$identity;
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$core$Task$init = elm$core$Task$succeed(0);
var elm$core$Task$andThen = _Scheduler_andThen;
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
		var task = _n0;
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
				return 0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0;
		return A2(elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			A2(elm$core$Task$map, toMessage, task));
	});
var elm$core$Basics$compare = _Utils_compare;
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
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
var elm$core$Dict$Black = 1;
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Dict$Red = 0;
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
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
					0,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1) {
				case 0:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
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
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
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
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
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
				0,
				rlK,
				rlV,
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5(elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
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
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
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
				0,
				lK,
				lV,
				A5(elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
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
			if (clr === 1) {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5(elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
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
				A5(elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_n2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
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
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
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
				if (_n4.$ === -1) {
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
		if (dict.$ === -2) {
			return elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _n4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
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
						if (_n7.$ === -1) {
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
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _n1 = elm$core$Dict$getMin(right);
				if (_n1.$ === -1) {
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
		if ((_n0.$ === -1) && (!_n0.a)) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _n0 = alter(
			A2(elm$core$Dict$get, targetKey, dictionary));
		if (!_n0.$) {
			var value = _n0.a;
			return A3(elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2(elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return elm$core$Result$Err(e);
		}
	});
var elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var elm$http$Http$NetworkError_ = {$: 2};
var elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$Timeout_ = {$: 1};
var elm$http$Http$emptyBody = _Http_emptyBody;
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			elm$core$Basics$identity,
			A2(elm$core$Basics$composeR, toResult, toMsg));
	});
var elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return elm$core$Result$Err(
				f(e));
		}
	});
var elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$NetworkError = {$: 2};
var elm$http$Http$Timeout = {$: 1};
var elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadUrl(url));
			case 1:
				return elm$core$Result$Err(elm$http$Http$Timeout);
			case 2:
				return elm$core$Result$Err(elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return elm$core$Result$Err(
					elm$http$Http$BadStatus(metadata.bq));
			default:
				var body = response.b;
				return A2(
					elm$core$Result$mapError,
					elm$http$Http$BadBody,
					toResult(body));
		}
	});
var elm$http$Http$expectString = function (toMsg) {
	return A2(
		elm$http$Http$expectStringResponse,
		toMsg,
		elm$http$Http$resolve(elm$core$Result$Ok));
};
var elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var elm$http$Http$State = F2(
	function (reqs, subs) {
		return {bg: reqs, bt: subs};
	});
var elm$http$Http$init = elm$core$Task$succeed(
	A2(elm$http$Http$State, elm$core$Dict$empty, _List_Nil));
var elm$core$Process$kill = _Scheduler_kill;
var elm$core$Process$spawn = _Scheduler_spawn;
var elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _n2 = A2(elm$core$Dict$get, tracker, reqs);
					if (_n2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _n2.a;
						return A2(
							elm$core$Task$andThen,
							function (_n3) {
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2(elm$core$Dict$remove, tracker, reqs));
							},
							elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						elm$core$Task$andThen,
						function (pid) {
							var _n4 = req.cm;
							if (_n4.$ === 1) {
								return A3(elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _n4.a;
								return A3(
									elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3(elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			elm$core$Task$andThen,
			function (reqs) {
				return elm$core$Task$succeed(
					A2(elm$http$Http$State, reqs, subs));
			},
			A3(elm$http$Http$updateReqs, router, cmds, state.bg));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (!_n0.$) {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _n0) {
		var actualTracker = _n0.a;
		var toMsg = _n0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? elm$core$Maybe$Just(
			A2(
				elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : elm$core$Maybe$Nothing;
	});
var elm$http$Http$onSelfMsg = F3(
	function (router, _n0, state) {
		var tracker = _n0.a;
		var progress = _n0.b;
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$filterMap,
					A3(elm$http$Http$maybeSend, router, tracker, progress),
					state.bt)));
	});
var elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return elm$http$Http$Request(
				{
					K: r.K,
					bB: r.bB,
					bJ: A2(_Http_mapExpect, func, r.bJ),
					bR: r.bR,
					bW: r.bW,
					cl: r.cl,
					cm: r.cm,
					S: r.S
				});
		}
	});
var elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$http$Http$subMap = F2(
	function (func, _n0) {
		var tracker = _n0.a;
		var toMsg = _n0.b;
		return A2(
			elm$http$Http$MySub,
			tracker,
			A2(elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager(elm$http$Http$init, elm$http$Http$onEffects, elm$http$Http$onSelfMsg, elm$http$Http$cmdMap, elm$http$Http$subMap);
var elm$http$Http$command = _Platform_leaf('Http');
var elm$http$Http$subscription = _Platform_leaf('Http');
var elm$http$Http$request = function (r) {
	return elm$http$Http$command(
		elm$http$Http$Request(
			{K: false, bB: r.bB, bJ: r.bJ, bR: r.bR, bW: r.bW, cl: r.cl, cm: r.cm, S: r.S}));
};
var elm$json$Json$Decode$decodeValue = _Json_run;
var elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var elm$time$Time$customZone = elm$time$Time$Zone;
var elm$time$Time$here = _Time_here(0);
var author$project$Main$init = function (savedModel) {
	return _Utils_Tuple2(
		A2(
			elm$core$Result$withDefault,
			author$project$Main$emptyModel,
			A2(elm$json$Json$Decode$decodeValue, author$project$Main$decodeModel, savedModel)),
		elm$core$Platform$Cmd$batch(
			A2(
				elm$core$List$cons,
				A2(elm$core$Task$perform, author$project$Main$TimeZone, elm$time$Time$here),
				A2(
					elm$core$List$map,
					function (wl) {
						return elm$http$Http$request(
							{
								bB: elm$http$Http$emptyBody,
								bJ: elm$http$Http$expectString(
									author$project$Main$GotWordlist(wl)),
								bR: _List_Nil,
								bW: 'GET',
								cl: elm$core$Maybe$Nothing,
								cm: elm$core$Maybe$Just(wl.S),
								S: wl.S
							});
					},
					author$project$Main$wordlists))));
};
var author$project$Main$SolverResults = function (a) {
	return {$: 17, a: a};
};
var author$project$Main$SolverStateChanged = function (a) {
	return {$: 18, a: a};
};
var author$project$Main$UpdateRecvProgress = F2(
	function (a, b) {
		return {$: 21, a: a, b: b};
	});
var elm$json$Json$Decode$value = _Json_decodeValue;
var author$project$Main$solverResults = _Platform_incomingPort('solverResults', elm$json$Json$Decode$value);
var author$project$Main$solverStateChanged = _Platform_incomingPort('solverStateChanged', elm$json$Json$Decode$value);
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$http$Http$track = F2(
	function (tracker, toMsg) {
		return elm$http$Http$subscription(
			A2(elm$http$Http$MySub, tracker, toMsg));
	});
var author$project$Main$subscriptions = function (model) {
	return elm$core$Platform$Sub$batch(
		A2(
			elm$core$List$cons,
			author$project$Main$solverStateChanged(author$project$Main$SolverStateChanged),
			A2(
				elm$core$List$cons,
				author$project$Main$solverResults(author$project$Main$SolverResults),
				A2(
					elm$core$List$map,
					function (wl) {
						return A2(
							elm$http$Http$track,
							wl.S,
							author$project$Main$UpdateRecvProgress(wl.ap));
					},
					author$project$Main$wordlists))));
};
var author$project$Main$All = 1;
var author$project$Main$ClearProgress = function (a) {
	return {$: 22, a: a};
};
var author$project$Main$CurrentPuzzle = 0;
var author$project$Main$Save = F2(
	function (a, b) {
		return {$: 9, a: a, b: b};
	});
var elm$time$Time$now = _Time_now(elm$time$Time$millisToPosix);
var author$project$Main$andSave = F2(
	function (mode, model) {
		return _Utils_Tuple2(
			model,
			A2(
				elm$core$Task$perform,
				author$project$Main$Save(mode),
				elm$time$Time$now));
	});
var author$project$Main$asCurrentPuzzleIn = F2(
	function (model, puzzle) {
		return _Utils_update(
			model,
			{e: puzzle});
	});
var author$project$Main$asSelectedPuzzleIn = F2(
	function (model, puzzle) {
		return _Utils_update(
			model,
			{I: puzzle});
	});
var author$project$Main$asSolverStateIn = F2(
	function (model, solverState) {
		return _Utils_update(
			model,
			{ao: solverState});
	});
var author$project$Main$clearProgress = F2(
	function (src, model) {
		return _Utils_update(
			model,
			{
				A: A2(elm$core$Dict$remove, src, model.A)
			});
	});
var author$project$Main$clearSelectedPuzzle = function (model) {
	return _Utils_update(
		model,
		{I: elm$core$Maybe$Nothing});
};
var elm$time$Time$posixToMillis = function (_n0) {
	var millis = _n0;
	return millis;
};
var author$project$Puzzle$comparePuzzles = F2(
	function (puz1, puz2) {
		return A2(
			elm$core$Basics$compare,
			elm$time$Time$posixToMillis(puz1.B),
			elm$time$Time$posixToMillis(puz2.B));
	});
var author$project$Util$insertWith = F3(
	function (cmp, x, l) {
		if (!l.b) {
			return _List_fromArray(
				[x]);
		} else {
			var y = l.a;
			var rest = l.b;
			var _n1 = A2(cmp, x, y);
			switch (_n1) {
				case 0:
					return A2(
						elm$core$List$cons,
						x,
						A2(elm$core$List$cons, y, rest));
				case 1:
					return A2(
						elm$core$List$cons,
						x,
						A2(elm$core$List$cons, y, rest));
				default:
					return A2(
						elm$core$List$cons,
						y,
						A3(author$project$Util$insertWith, cmp, x, rest));
			}
		}
	});
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
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
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var author$project$Main$trySave = F2(
	function (puzzle, savedPuzzles) {
		return A2(
			elm$core$List$all,
			elm$core$String$isEmpty,
			_List_fromArray(
				[puzzle.bv, puzzle.aJ])) ? savedPuzzles : A3(author$project$Util$insertWith, author$project$Puzzle$comparePuzzles, puzzle, savedPuzzles);
	});
var author$project$Main$withSolverResult = F2(
	function (mResult, model) {
		return _Utils_update(
			model,
			{an: mResult});
	});
var author$project$Puzzle$samePuzzle = F2(
	function (puz1, puz2) {
		return A2(author$project$Puzzle$comparePuzzles, puz1, puz2) === 1;
	});
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
var author$project$Main$loadPuzzle = F2(
	function (puzzle, model) {
		return A2(
			author$project$Main$withSolverResult,
			elm$core$Maybe$Nothing,
			_Utils_update(
				model,
				{
					e: puzzle,
					s: A2(
						author$project$Main$trySave,
						model.e,
						A2(
							elm$core$List$filter,
							A2(
								elm$core$Basics$composeL,
								elm$core$Basics$not,
								author$project$Puzzle$samePuzzle(puzzle)),
							model.s)),
					t: _List_Nil
				}));
	});
var author$project$Main$pendingDeletion = F2(
	function (pending, model) {
		return _Utils_update(
			model,
			{_: pending});
	});
var author$project$Main$popCurrentPuzzle = F2(
	function (newPuzzle, model) {
		return A2(
			author$project$Main$withSolverResult,
			elm$core$Maybe$Nothing,
			_Utils_update(
				model,
				{
					e: newPuzzle,
					s: A2(author$project$Main$trySave, model.e, model.s)
				}));
	});
var author$project$Main$saveCurrentPuzzle = _Platform_outgoingPort('saveCurrentPuzzle', elm$core$Basics$identity);
var author$project$Main$savePuzzles = _Platform_outgoingPort('savePuzzles', elm$core$Basics$identity);
var author$project$Main$solveNumbering = _Platform_outgoingPort('solveNumbering', elm$core$Basics$identity);
var author$project$Util$updateIndex = F3(
	function (index, f, l) {
		if (!l.b) {
			return _List_Nil;
		} else {
			var x = l.a;
			var rest = l.b;
			return (!index) ? A2(
				elm$core$List$cons,
				f(x),
				rest) : A2(
				elm$core$List$cons,
				x,
				A3(author$project$Util$updateIndex, index - 1, f, rest));
		}
	});
var author$project$Puzzle$updateNumbering = F4(
	function (index, numIndex, mQuoteNum, puzzle) {
		return _Utils_update(
			puzzle,
			{
				W: A3(
					author$project$Util$updateIndex,
					index,
					function (clue) {
						return _Utils_update(
							clue,
							{
								ag: A3(
									author$project$Util$updateIndex,
									numIndex,
									function (_n0) {
										var c = _n0.b;
										return _Utils_Tuple2(mQuoteNum, c);
									},
									clue.ag)
							});
					},
					puzzle.W)
			});
	});
var author$project$Solver$applySMTNumbering = F2(
	function (nums, puz) {
		var apply = F2(
			function (num, newPuz) {
				return A4(
					author$project$Puzzle$updateNumbering,
					num.au,
					num.aw,
					elm$core$Maybe$Just(num.az),
					newPuz);
			});
		return A3(elm$core$List$foldr, apply, puz, nums);
	});
var author$project$Main$tryApplySMTNumberingTo = F2(
	function (model, result) {
		return A2(
			author$project$Main$withSolverResult,
			elm$core$Maybe$Just(result),
			function () {
				var _n0 = result.ag;
				switch (_n0.$) {
					case 2:
						return model;
					case 1:
						return model;
					default:
						var nums = _n0.a;
						return A2(
							author$project$Main$asCurrentPuzzleIn,
							model,
							A2(author$project$Solver$applySMTNumbering, nums, model.e));
				}
			}());
	});
var author$project$Main$updateProgress = F3(
	function (src, progress, model) {
		return _Utils_update(
			model,
			{
				A: A3(elm$core$Dict$insert, src, progress, model.A)
			});
	});
var author$project$Puzzle$clearNumbering = function (puzzle) {
	return _Utils_update(
		puzzle,
		{
			W: A2(
				elm$core$List$map,
				function (clue) {
					return _Utils_update(
						clue,
						{
							ag: A2(
								elm$core$List$map,
								function (_n0) {
									var c = _n0.b;
									return _Utils_Tuple2(elm$core$Maybe$Nothing, c);
								},
								clue.ag)
						});
				},
				puzzle.W)
		});
};
var elm$json$Json$Encode$null = _Json_encodeNull;
var author$project$Util$encodeNullable = F2(
	function (encode, ma) {
		if (ma.$ === 1) {
			return elm$json$Json$Encode$null;
		} else {
			var a = ma.a;
			return encode(a);
		}
	});
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var elm$json$Json$Encode$int = _Json_wrap;
var elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			elm$core$List$foldl,
			F2(
				function (_n0, obj) {
					var k = _n0.a;
					var v = _n0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var elm$json$Json$Encode$string = _Json_wrap;
var author$project$Puzzle$encodeAnswer = function (_n0) {
	var mNum = _n0.a;
	var c = _n0.b;
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'number',
				A2(author$project$Util$encodeNullable, elm$json$Json$Encode$int, mNum)),
				_Utils_Tuple2(
				'char',
				elm$json$Json$Encode$string(
					elm$core$String$fromChar(c)))
			]));
};
var elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var author$project$Puzzle$encodeClue = function (clue) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'hint',
				elm$json$Json$Encode$string(clue.aY)),
				_Utils_Tuple2(
				'text',
				elm$json$Json$Encode$string(clue.cj)),
				_Utils_Tuple2(
				'answer',
				A2(elm$json$Json$Encode$list, author$project$Puzzle$encodeAnswer, clue.ag))
			]));
};
var author$project$Puzzle$encodePhase = function (phase) {
	return elm$json$Json$Encode$string(
		function () {
			switch (phase) {
				case 0:
					return 'QuoteEntry';
				case 1:
					return 'Anagramming';
				default:
					return 'CluingLettering';
			}
		}());
};
var author$project$Puzzle$encodePuzzle = function (puzzle) {
	return elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'title',
				elm$json$Json$Encode$string(puzzle.bv)),
				_Utils_Tuple2(
				'author',
				elm$json$Json$Encode$string(puzzle.aJ)),
				_Utils_Tuple2(
				'quote',
				elm$json$Json$Encode$string(puzzle.aC)),
				_Utils_Tuple2(
				'clues',
				A2(elm$json$Json$Encode$list, author$project$Puzzle$encodeClue, puzzle.W)),
				_Utils_Tuple2(
				'phase',
				author$project$Puzzle$encodePhase(puzzle.H)),
				_Utils_Tuple2(
				'timeModified',
				elm$json$Json$Encode$int(
					elm$time$Time$posixToMillis(puzzle.B)))
			]));
};
var elm$core$String$fromList = _String_fromList;
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var author$project$Puzzle$clueAnswer = function (c) {
	return elm$core$String$fromList(
		A2(elm$core$List$map, elm$core$Tuple$second, c.ag));
};
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var elm$core$String$toUpper = _String_toUpper;
var author$project$Puzzle$defaultClue = function (s) {
	return {
		ag: A2(
			elm$core$List$map,
			elm$core$Tuple$pair(elm$core$Maybe$Nothing),
			elm$core$String$toList(s)),
		aY: '',
		cj: elm$core$String$toUpper(s)
	};
};
var elm$core$String$filter = _String_filter;
var author$project$Puzzle$initialism = function (puzzle) {
	return A2(
		elm$core$String$filter,
		elm$core$Char$isAlphaNum,
		_Utils_ap(puzzle.aJ, puzzle.bv));
};
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$String$startsWith = _String_startsWith;
var author$project$Puzzle$fixupAnswerInitials = function (puzzle) {
	var initials = A2(
		elm$core$List$map,
		elm$core$String$fromChar,
		elm$core$String$toList(
			author$project$Puzzle$initialism(puzzle)));
	var clues = (!_Utils_eq(
		elm$core$List$length(puzzle.W),
		elm$core$List$length(initials))) ? A2(elm$core$List$map, author$project$Puzzle$defaultClue, initials) : A3(
		elm$core$List$map2,
		F2(
			function (i, c) {
				return A2(
					elm$core$String$startsWith,
					i,
					author$project$Puzzle$clueAnswer(c)) ? c : author$project$Puzzle$defaultClue(i);
			}),
		initials,
		puzzle.W);
	return _Utils_update(
		puzzle,
		{W: clues});
};
var author$project$Puzzle$setAuthor = F2(
	function (author, puzzle) {
		return _Utils_update(
			puzzle,
			{aJ: author});
	});
var author$project$Puzzle$setPhase = F2(
	function (phase, puzzle) {
		return _Utils_update(
			puzzle,
			{H: phase});
	});
var author$project$Puzzle$setQuote = F2(
	function (quote, puzzle) {
		return _Utils_update(
			puzzle,
			{aC: quote});
	});
var author$project$Puzzle$setTimeModified = F2(
	function (now, puzzle) {
		return _Utils_update(
			puzzle,
			{B: now});
	});
var author$project$Puzzle$setTitle = F2(
	function (title, puzzle) {
		return _Utils_update(
			puzzle,
			{bv: title});
	});
var author$project$Util$cleanString = function (s) {
	return A2(
		elm$core$String$filter,
		elm$core$Char$isAlphaNum,
		elm$core$String$toUpper(s));
};
var author$project$Util$cleanChars = function (s) {
	return elm$core$String$toList(
		author$project$Util$cleanString(s));
};
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var author$project$Puzzle$quoteIndex = F2(
	function (puzzle, index) {
		return elm$core$List$head(
			A2(
				elm$core$List$drop,
				index,
				author$project$Util$cleanChars(puzzle.aC)));
	});
var elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2(elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var elm$core$List$repeat = F2(
	function (n, value) {
		return A3(elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var elm$core$String$length = _String_length;
var author$project$Puzzle$updateAnswer = F3(
	function (index, answer, puzzle) {
		return _Utils_update(
			puzzle,
			{
				W: A3(
					author$project$Util$updateIndex,
					index,
					function (clue) {
						var numbering = A2(elm$core$List$map, elm$core$Tuple$first, clue.ag);
						var extendedNumbering = _Utils_ap(
							numbering,
							A2(
								elm$core$List$repeat,
								elm$core$String$length(answer) - elm$core$List$length(numbering),
								elm$core$Maybe$Nothing));
						var numberedAnswer = A3(
							elm$core$List$map2,
							F2(
								function (mnum, c) {
									if (mnum.$ === 1) {
										return _Utils_Tuple2(elm$core$Maybe$Nothing, c);
									} else {
										var num = mnum.a;
										return _Utils_eq(
											A2(author$project$Puzzle$quoteIndex, puzzle, num),
											elm$core$Maybe$Just(c)) ? _Utils_Tuple2(
											elm$core$Maybe$Just(num),
											c) : _Utils_Tuple2(elm$core$Maybe$Nothing, c);
									}
								}),
							extendedNumbering,
							elm$core$String$toList(
								A2(
									elm$core$String$filter,
									elm$core$Char$isAlphaNum,
									elm$core$String$toUpper(answer))));
						return _Utils_update(
							clue,
							{ag: numberedAnswer, cj: answer});
					},
					puzzle.W)
			});
	});
var author$project$Puzzle$updateHint = F3(
	function (index, hint, puzzle) {
		return _Utils_update(
			puzzle,
			{
				W: A3(
					author$project$Util$updateIndex,
					index,
					function (clue) {
						return _Utils_update(
							clue,
							{aY: hint});
					},
					puzzle.W)
			});
	});
var author$project$SMT$SolverDownloading = 1;
var author$project$SMT$SolverInitializing = 2;
var author$project$SMT$SolverReady = 3;
var author$project$SMT$SolverRunning = 4;
var author$project$SMT$decodeSolverState = A2(
	elm$json$Json$Decode$andThen,
	function (s) {
		switch (s) {
			case 'SolverUnloaded':
				return elm$json$Json$Decode$succeed(0);
			case 'SolverDownloading':
				return elm$json$Json$Decode$succeed(1);
			case 'SolverInitializing':
				return elm$json$Json$Decode$succeed(2);
			case 'SolverReady':
				return elm$json$Json$Decode$succeed(3);
			case 'SolverRunning':
				return elm$json$Json$Decode$succeed(4);
			default:
				return elm$json$Json$Decode$fail('expected solver state, found \'' + (s + '\''));
		}
	},
	elm$json$Json$Decode$string);
var author$project$Solver$SMTFailed = {$: 2};
var author$project$Solver$SMTOk = function (a) {
	return {$: 0, a: a};
};
var author$project$Solver$SMTTimeout = {$: 1};
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var elm$parser$Parser$Advanced$Parser = elm$core$Basics$identity;
var elm$parser$Parser$Advanced$map2 = F3(
	function (func, _n0, _n1) {
		var parseA = _n0;
		var parseB = _n1;
		return function (s0) {
			var _n2 = parseA(s0);
			if (_n2.$ === 1) {
				var p = _n2.a;
				var x = _n2.b;
				return A2(elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _n2.a;
				var a = _n2.b;
				var s1 = _n2.c;
				var _n3 = parseB(s1);
				if (_n3.$ === 1) {
					var p2 = _n3.a;
					var x = _n3.b;
					return A2(elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _n3.a;
					var b = _n3.b;
					var s2 = _n3.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$always, keepParser, ignoreParser);
	});
var elm$parser$Parser$ignorer = elm$parser$Parser$Advanced$ignorer;
var elm$parser$Parser$ExpectingInt = {$: 1};
var elm$parser$Parser$Advanced$consumeBase = _Parser_consumeBase;
var elm$parser$Parser$Advanced$consumeBase16 = _Parser_consumeBase16;
var elm$core$String$slice = _String_slice;
var elm$core$String$toFloat = _String_toFloat;
var elm$parser$Parser$Advanced$bumpOffset = F2(
	function (newOffset, s) {
		return {ah: s.ah + (newOffset - s.b), c: s.c, d: s.d, b: newOffset, aa: s.aa, a: s.a};
	});
var elm$parser$Parser$Advanced$chompBase10 = _Parser_chompBase10;
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$parser$Parser$Advanced$isAsciiCode = _Parser_isAsciiCode;
var elm$parser$Parser$Advanced$consumeExp = F2(
	function (offset, src) {
		if (A3(elm$parser$Parser$Advanced$isAsciiCode, 101, offset, src) || A3(elm$parser$Parser$Advanced$isAsciiCode, 69, offset, src)) {
			var eOffset = offset + 1;
			var expOffset = (A3(elm$parser$Parser$Advanced$isAsciiCode, 43, eOffset, src) || A3(elm$parser$Parser$Advanced$isAsciiCode, 45, eOffset, src)) ? (eOffset + 1) : eOffset;
			var newOffset = A2(elm$parser$Parser$Advanced$chompBase10, expOffset, src);
			return _Utils_eq(expOffset, newOffset) ? (-newOffset) : newOffset;
		} else {
			return offset;
		}
	});
var elm$parser$Parser$Advanced$consumeDotAndExp = F2(
	function (offset, src) {
		return A3(elm$parser$Parser$Advanced$isAsciiCode, 46, offset, src) ? A2(
			elm$parser$Parser$Advanced$consumeExp,
			A2(elm$parser$Parser$Advanced$chompBase10, offset + 1, src),
			src) : A2(elm$parser$Parser$Advanced$consumeExp, offset, src);
	});
var elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {ah: col, bF: contextStack, ba: problem, aa: row};
	});
var elm$parser$Parser$Advanced$Empty = {$: 0};
var elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			elm$parser$Parser$Advanced$AddRight,
			elm$parser$Parser$Advanced$Empty,
			A4(elm$parser$Parser$Advanced$DeadEnd, s.aa, s.ah, x, s.c));
	});
var elm$parser$Parser$Advanced$finalizeInt = F5(
	function (invalid, handler, startOffset, _n0, s) {
		var endOffset = _n0.a;
		var n = _n0.b;
		if (handler.$ === 1) {
			var x = handler.a;
			return A2(
				elm$parser$Parser$Advanced$Bad,
				true,
				A2(elm$parser$Parser$Advanced$fromState, s, x));
		} else {
			var toValue = handler.a;
			return _Utils_eq(startOffset, endOffset) ? A2(
				elm$parser$Parser$Advanced$Bad,
				_Utils_cmp(s.b, startOffset) < 0,
				A2(elm$parser$Parser$Advanced$fromState, s, invalid)) : A3(
				elm$parser$Parser$Advanced$Good,
				true,
				toValue(n),
				A2(elm$parser$Parser$Advanced$bumpOffset, endOffset, s));
		}
	});
var elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			elm$parser$Parser$Advanced$AddRight,
			elm$parser$Parser$Advanced$Empty,
			A4(elm$parser$Parser$Advanced$DeadEnd, row, col, x, context));
	});
var elm$parser$Parser$Advanced$finalizeFloat = F6(
	function (invalid, expecting, intSettings, floatSettings, intPair, s) {
		var intOffset = intPair.a;
		var floatOffset = A2(elm$parser$Parser$Advanced$consumeDotAndExp, intOffset, s.a);
		if (floatOffset < 0) {
			return A2(
				elm$parser$Parser$Advanced$Bad,
				true,
				A4(elm$parser$Parser$Advanced$fromInfo, s.aa, s.ah - (floatOffset + s.b), invalid, s.c));
		} else {
			if (_Utils_eq(s.b, floatOffset)) {
				return A2(
					elm$parser$Parser$Advanced$Bad,
					false,
					A2(elm$parser$Parser$Advanced$fromState, s, expecting));
			} else {
				if (_Utils_eq(intOffset, floatOffset)) {
					return A5(elm$parser$Parser$Advanced$finalizeInt, invalid, intSettings, s.b, intPair, s);
				} else {
					if (floatSettings.$ === 1) {
						var x = floatSettings.a;
						return A2(
							elm$parser$Parser$Advanced$Bad,
							true,
							A2(elm$parser$Parser$Advanced$fromState, s, invalid));
					} else {
						var toValue = floatSettings.a;
						var _n1 = elm$core$String$toFloat(
							A3(elm$core$String$slice, s.b, floatOffset, s.a));
						if (_n1.$ === 1) {
							return A2(
								elm$parser$Parser$Advanced$Bad,
								true,
								A2(elm$parser$Parser$Advanced$fromState, s, invalid));
						} else {
							var n = _n1.a;
							return A3(
								elm$parser$Parser$Advanced$Good,
								true,
								toValue(n),
								A2(elm$parser$Parser$Advanced$bumpOffset, floatOffset, s));
						}
					}
				}
			}
		}
	});
var elm$parser$Parser$Advanced$number = function (c) {
	return function (s) {
		if (A3(elm$parser$Parser$Advanced$isAsciiCode, 48, s.b, s.a)) {
			var zeroOffset = s.b + 1;
			var baseOffset = zeroOffset + 1;
			return A3(elm$parser$Parser$Advanced$isAsciiCode, 120, zeroOffset, s.a) ? A5(
				elm$parser$Parser$Advanced$finalizeInt,
				c.bV,
				c.aX,
				baseOffset,
				A2(elm$parser$Parser$Advanced$consumeBase16, baseOffset, s.a),
				s) : (A3(elm$parser$Parser$Advanced$isAsciiCode, 111, zeroOffset, s.a) ? A5(
				elm$parser$Parser$Advanced$finalizeInt,
				c.bV,
				c.a6,
				baseOffset,
				A3(elm$parser$Parser$Advanced$consumeBase, 8, baseOffset, s.a),
				s) : (A3(elm$parser$Parser$Advanced$isAsciiCode, 98, zeroOffset, s.a) ? A5(
				elm$parser$Parser$Advanced$finalizeInt,
				c.bV,
				c.aK,
				baseOffset,
				A3(elm$parser$Parser$Advanced$consumeBase, 2, baseOffset, s.a),
				s) : A6(
				elm$parser$Parser$Advanced$finalizeFloat,
				c.bV,
				c.aS,
				c.a$,
				c.aT,
				_Utils_Tuple2(zeroOffset, 0),
				s)));
		} else {
			return A6(
				elm$parser$Parser$Advanced$finalizeFloat,
				c.bV,
				c.aS,
				c.a$,
				c.aT,
				A3(elm$parser$Parser$Advanced$consumeBase, 10, s.b, s.a),
				s);
		}
	};
};
var elm$parser$Parser$Advanced$int = F2(
	function (expecting, invalid) {
		return elm$parser$Parser$Advanced$number(
			{
				aK: elm$core$Result$Err(invalid),
				aS: expecting,
				aT: elm$core$Result$Err(invalid),
				aX: elm$core$Result$Err(invalid),
				a$: elm$core$Result$Ok(elm$core$Basics$identity),
				bV: invalid,
				a6: elm$core$Result$Err(invalid)
			});
	});
var elm$parser$Parser$int = A2(elm$parser$Parser$Advanced$int, elm$parser$Parser$ExpectingInt, elm$parser$Parser$ExpectingInt);
var elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$apL, parseFunc, parseArg);
	});
var elm$parser$Parser$keeper = elm$parser$Parser$Advanced$keeper;
var elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3(elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.a);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.b, offset) < 0,
					0,
					{ah: col, c: s0.c, d: s0.d, b: offset, aa: row, a: s0.a});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return function (s) {
		return A5(elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.b, s.aa, s.ah, s);
	};
};
var elm$parser$Parser$Advanced$spaces = elm$parser$Parser$Advanced$chompWhile(
	function (c) {
		return (c === ' ') || ((c === '\n') || (c === '\r'));
	});
var elm$parser$Parser$spaces = elm$parser$Parser$Advanced$spaces;
var elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3(elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var elm$parser$Parser$succeed = elm$parser$Parser$Advanced$succeed;
var elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 8, a: a};
};
var elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var elm$parser$Parser$Advanced$token = function (_n0) {
	var str = _n0.a;
	var expecting = _n0.b;
	var progress = !elm$core$String$isEmpty(str);
	return function (s) {
		var _n1 = A5(elm$parser$Parser$Advanced$isSubString, str, s.b, s.aa, s.ah, s.a);
		var newOffset = _n1.a;
		var newRow = _n1.b;
		var newCol = _n1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			elm$parser$Parser$Advanced$Bad,
			false,
			A2(elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{ah: newCol, c: s.c, d: s.d, b: newOffset, aa: newRow, a: s.a});
	};
};
var elm$parser$Parser$Advanced$symbol = elm$parser$Parser$Advanced$token;
var elm$parser$Parser$symbol = function (str) {
	return elm$parser$Parser$Advanced$symbol(
		A2(
			elm$parser$Parser$Advanced$Token,
			str,
			elm$parser$Parser$ExpectingSymbol(str)));
};
var author$project$Solver$smtValueParser = A2(
	elm$parser$Parser$keeper,
	A2(
		elm$parser$Parser$keeper,
		A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$succeed(
							F3(
								function (clue, letter, number) {
									return {au: clue, aw: letter, az: number};
								})),
						elm$parser$Parser$spaces),
					elm$parser$Parser$symbol('(')),
				elm$parser$Parser$symbol('clue')),
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$int,
				elm$parser$Parser$symbol('_letter'))),
		A2(elm$parser$Parser$ignorer, elm$parser$Parser$int, elm$parser$Parser$spaces)),
	A2(
		elm$parser$Parser$ignorer,
		elm$parser$Parser$int,
		elm$parser$Parser$symbol(')')));
var elm$parser$Parser$Advanced$lazy = function (thunk) {
	return function (s) {
		var _n0 = thunk(0);
		var parse = _n0;
		return parse(s);
	};
};
var elm$parser$Parser$lazy = elm$parser$Parser$Advanced$lazy;
var elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2(elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _n1 = parse(s0);
				if (!_n1.$) {
					var step = _n1;
					return step;
				} else {
					var step = _n1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2(elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3(elm$parser$Parser$Advanced$oneOfHelp, s, elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var elm$parser$Parser$oneOf = elm$parser$Parser$Advanced$oneOf;
var author$project$Util$listOf = function (item) {
	return elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				elm$parser$Parser$keeper,
				A2(
					elm$parser$Parser$keeper,
					A2(
						elm$parser$Parser$ignorer,
						elm$parser$Parser$succeed(
							F2(
								function (hd, tl) {
									return A2(elm$core$List$cons, hd, tl);
								})),
						elm$parser$Parser$spaces),
					A2(elm$parser$Parser$ignorer, item, elm$parser$Parser$spaces)),
				elm$parser$Parser$lazy(
					function (_n0) {
						return author$project$Util$listOf(item);
					})),
				elm$parser$Parser$succeed(_List_Nil)
			]));
};
var elm$parser$Parser$ExpectingEnd = {$: 10};
var elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			elm$core$String$length(s.a),
			s.b) ? A3(elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			elm$parser$Parser$Advanced$Bad,
			false,
			A2(elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var elm$parser$Parser$end = elm$parser$Parser$Advanced$end(elm$parser$Parser$ExpectingEnd);
var author$project$Solver$smtModelParser = A2(
	elm$parser$Parser$keeper,
	A2(
		elm$parser$Parser$ignorer,
		A2(
			elm$parser$Parser$ignorer,
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed(elm$core$Basics$identity),
				elm$parser$Parser$spaces),
			elm$parser$Parser$symbol('(')),
		elm$parser$Parser$spaces),
	A2(
		elm$parser$Parser$ignorer,
		A2(
			elm$parser$Parser$ignorer,
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					author$project$Util$listOf(author$project$Solver$smtValueParser),
					elm$parser$Parser$spaces),
				elm$parser$Parser$symbol(')')),
			elm$parser$Parser$spaces),
		elm$parser$Parser$end));
var author$project$Solver$smtAnswerParser = elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			elm$parser$Parser$ignorer,
			elm$parser$Parser$succeed(author$project$Solver$SMTFailed),
			elm$parser$Parser$symbol('unsat')),
			A2(
			elm$parser$Parser$ignorer,
			elm$parser$Parser$succeed(author$project$Solver$SMTTimeout),
			elm$parser$Parser$symbol('unknown')),
			A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$ignorer,
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$succeed(author$project$Solver$SMTOk),
					elm$parser$Parser$symbol('sat')),
				elm$parser$Parser$spaces),
			author$project$Solver$smtModelParser)
		]));
var elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {ah: col, ba: problem, aa: row};
	});
var elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3(elm$parser$Parser$DeadEnd, p.aa, p.ah, p.ba);
};
var elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var elm$parser$Parser$Advanced$run = F2(
	function (_n0, src) {
		var parse = _n0;
		var _n1 = parse(
			{ah: 1, c: _List_Nil, d: 1, b: 0, aa: 1, a: src});
		if (!_n1.$) {
			var value = _n1.b;
			return elm$core$Result$Ok(value);
		} else {
			var bag = _n1.b;
			return elm$core$Result$Err(
				A2(elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var elm$parser$Parser$run = F2(
	function (parser, source) {
		var _n0 = A2(elm$parser$Parser$Advanced$run, parser, source);
		if (!_n0.$) {
			var a = _n0.a;
			return elm$core$Result$Ok(a);
		} else {
			var problems = _n0.a;
			return elm$core$Result$Err(
				A2(elm$core$List$map, elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var author$project$Solver$decodeSMTResult = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (elapsed, stdout) {
			var answer = A2(
				elm$core$Result$withDefault,
				author$project$Solver$SMTFailed,
				A2(
					elm$parser$Parser$run,
					author$project$Solver$smtAnswerParser,
					A2(elm$core$String$join, '\n', stdout)));
			return {ag: answer, aj: elapsed};
		}),
	A2(elm$json$Json$Decode$field, 'elapsed', elm$json$Json$Decode$int),
	A2(
		elm$json$Json$Decode$field,
		'stdout',
		elm$json$Json$Decode$list(elm$json$Json$Decode$string)));
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
var elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var elm$core$String$words = _String_words;
var author$project$Puzzle$quoteIndexWords = function (puzzle) {
	return elm$core$Dict$fromList(
		A2(
			elm$core$List$indexedMap,
			elm$core$Tuple$pair,
			elm$core$List$concat(
				A2(
					elm$core$List$indexedMap,
					F2(
						function (i, w) {
							return A2(
								elm$core$List$repeat,
								elm$core$List$length(w),
								i);
						}),
					A2(
						elm$core$List$filter,
						A2(elm$core$Basics$composeL, elm$core$Basics$not, elm$core$List$isEmpty),
						A2(
							elm$core$List$map,
							author$project$Util$cleanChars,
							elm$core$String$words(puzzle.aC)))))));
};
var author$project$Util$updateCons = F3(
	function (k, v, d) {
		return A3(
			elm$core$Dict$update,
			k,
			function (mvs) {
				if (mvs.$ === 1) {
					return elm$core$Maybe$Just(
						_List_fromArray(
							[v]));
				} else {
					var vs = mvs.a;
					return elm$core$Maybe$Just(
						A2(elm$core$List$cons, v, vs));
				}
			},
			d);
	});
var author$project$Puzzle$quoteIndices = function (puzzle) {
	return A3(
		elm$core$List$foldr,
		F2(
			function (_n0, d) {
				var c = _n0.a;
				var i = _n0.b;
				return A3(author$project$Util$updateCons, c, i, d);
			}),
		elm$core$Dict$empty,
		A2(
			elm$core$List$indexedMap,
			F2(
				function (i, c) {
					return _Utils_Tuple2(c, i);
				}),
			author$project$Util$cleanChars(puzzle.aC)));
};
var author$project$Solver$Distinct = function (a) {
	return {$: 2, a: a};
};
var author$project$Solver$IsInt = function (a) {
	return {$: 0, a: a};
};
var author$project$Solver$NotAscending = function (a) {
	return {$: 3, a: a};
};
var author$project$Solver$NotSameWord = function (a) {
	return {$: 4, a: a};
};
var author$project$Solver$OneOf = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var elm$core$Char$toUpper = _Char_toUpper;
var elm$core$Dict$values = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2(elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var elm$core$List$concatMap = F2(
	function (f, list) {
		return elm$core$List$concat(
			A2(elm$core$List$map, f, list));
	});
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var author$project$Solver$constraintsOfPuzzle = F2(
	function (qIndices, puzzle) {
		var varName = F2(
			function (clueIndex, numIndex) {
				return 'clue' + (elm$core$String$fromInt(clueIndex) + ('_' + ('letter' + elm$core$String$fromInt(numIndex))));
			});
		var clueVarsByClue = A2(
			elm$core$List$indexedMap,
			F2(
				function (clueIndex, clue) {
					return A2(
						elm$core$List$map,
						function (_n2) {
							var numIndex = _n2.a;
							var _n3 = _n2.b;
							var c = _n3.b;
							return _Utils_Tuple2(
								A2(varName, clueIndex, numIndex),
								c);
						},
						A2(elm$core$List$indexedMap, elm$core$Tuple$pair, clue.ag));
				}),
			puzzle.W);
		var numberingConstraints = A2(
			elm$core$List$concatMap,
			function (vs) {
				var vars = A2(elm$core$List$map, elm$core$Tuple$first, vs);
				return _List_fromArray(
					[
						author$project$Solver$NotAscending(vars),
						author$project$Solver$NotAscending(
						elm$core$List$reverse(vars)),
						author$project$Solver$NotSameWord(vars)
					]);
			},
			clueVarsByClue);
		var clueVars = elm$core$List$concat(clueVarsByClue);
		var charUses = A3(
			elm$core$List$foldr,
			F2(
				function (_n1, d) {
					var v = _n1.a;
					var c = _n1.b;
					return A3(author$project$Util$updateCons, c, v, d);
				}),
			elm$core$Dict$empty,
			clueVars);
		var disjointnessConstraints = A2(
			elm$core$List$map,
			author$project$Solver$Distinct,
			elm$core$Dict$values(charUses));
		var charConstraints = A2(
			elm$core$List$concatMap,
			function (_n0) {
				var v = _n0.a;
				var c = _n0.b;
				var uses = A2(
					elm$core$Maybe$withDefault,
					_List_Nil,
					A2(
						elm$core$Dict$get,
						elm$core$Char$toUpper(c),
						qIndices));
				return _List_fromArray(
					[
						author$project$Solver$IsInt(v),
						A2(author$project$Solver$OneOf, v, uses)
					]);
			},
			clueVars);
		return _Utils_ap(
			charConstraints,
			_Utils_ap(disjointnessConstraints, numberingConstraints));
	});
var author$project$SMT$and = function (props) {
	if (!props.b) {
		return 'true';
	} else {
		if (!props.b.b) {
			var prop = props.a;
			return prop;
		} else {
			return '(and ' + (A2(elm$core$String$join, ' ', props) + ')');
		}
	}
};
var author$project$SMT$assert = function (prop) {
	return '(assert ' + (prop + ')');
};
var author$project$SMT$wordFun = 'word-of';
var author$project$SMT$wordOf = function (x) {
	return '(' + (author$project$SMT$wordFun + (' ' + (x + ')')));
};
var author$project$Solver$isDefn = function (c) {
	if (!c.$) {
		return true;
	} else {
		return false;
	}
};
var author$project$SMT$ascending = function (vars) {
	if (!vars.b) {
		return 'true';
	} else {
		if (!vars.b.b) {
			var _var = vars.a;
			return 'true';
		} else {
			return '(< ' + (A2(elm$core$String$join, ' ', vars) + ')');
		}
	}
};
var author$project$SMT$distinct = function (vars) {
	if (!vars.b) {
		return 'true';
	} else {
		if (!vars.b.b) {
			var _var = vars.a;
			return 'true';
		} else {
			return '(distinct ' + (A2(elm$core$String$join, ' ', vars) + ')');
		}
	}
};
var author$project$SMT$eq = F2(
	function (l, r) {
		return '(= ' + (l + (' ' + (r + ')')));
	});
var author$project$SMT$not = function (prop) {
	return '(not ' + (prop + ')');
};
var author$project$SMT$or = function (props) {
	if (!props.b) {
		return 'true';
	} else {
		if (!props.b.b) {
			var prop = props.a;
			return prop;
		} else {
			return '(or ' + (A2(elm$core$String$join, ' ', props) + ')');
		}
	}
};
var author$project$Solver$smt2OfConstraint = function (c) {
	switch (c.$) {
		case 0:
			var _var = c.a;
			return '(declare-const ' + (_var + ' Int)');
		case 1:
			var _var = c.a;
			var ns = c.b;
			return author$project$SMT$assert(
				author$project$SMT$or(
					A2(
						elm$core$List$map,
						function (n) {
							return A2(
								author$project$SMT$eq,
								_var,
								elm$core$String$fromInt(n));
						},
						ns)));
		case 2:
			if (!c.a.b) {
				return author$project$SMT$assert('true');
			} else {
				var vars = c.a;
				return author$project$SMT$assert(
					author$project$SMT$distinct(vars));
			}
		case 3:
			var vars = c.a;
			return author$project$SMT$assert(
				author$project$SMT$not(
					author$project$SMT$ascending(vars)));
		default:
			if (!c.a.b) {
				return author$project$SMT$assert('true');
			} else {
				if (!c.a.b.b) {
					var _n1 = c.a;
					return author$project$SMT$assert('true');
				} else {
					var vars = c.a;
					return author$project$SMT$assert(
						author$project$SMT$distinct(
							A2(elm$core$List$map, author$project$SMT$wordOf, vars)));
				}
			}
	}
};
var elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _n0) {
				var trues = _n0.a;
				var falses = _n0.b;
				return pred(x) ? _Utils_Tuple2(
					A2(elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2(elm$core$List$cons, x, falses));
			});
		return A3(
			elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var author$project$Solver$smt2OfConstraints = F2(
	function (qIndexWords, constraints) {
		var wordFun = function () {
			var vals = author$project$SMT$assert(
				author$project$SMT$and(
					A3(
						elm$core$Dict$foldr,
						F3(
							function (x, wordNum, eqs) {
								return A2(
									elm$core$List$cons,
									'(= ' + (author$project$SMT$wordOf(
										elm$core$String$fromInt(x)) + (' ' + (elm$core$String$fromInt(wordNum) + ')'))),
									eqs);
							}),
						_List_Nil,
						qIndexWords)));
			var decl = '(declare-fun ' + (author$project$SMT$wordFun + ' (Int) Int)');
			var conds = A3(
				elm$core$Dict$foldr,
				F3(
					function (x, wordNum, otw) {
						return '(ite (= n ' + (elm$core$String$fromInt(x) + (') ' + (elm$core$String$fromInt(wordNum) + (' ' + (otw + ')')))));
					}),
				'-1',
				qIndexWords);
			var defn = '(define-fun ' + (author$project$SMT$wordFun + (' ((n Int)) Int ' + (conds + ')')));
			return _List_fromArray(
				[decl, vals]);
		}();
		var _n0 = A2(elm$core$List$partition, author$project$Solver$isDefn, constraints);
		var defnConstraints = _n0.a;
		var assertConstraints = _n0.b;
		var assertions = A2(elm$core$List$map, author$project$Solver$smt2OfConstraint, assertConstraints);
		var defns = A2(elm$core$List$map, author$project$Solver$smt2OfConstraint, defnConstraints);
		var vars = A2(
			elm$core$List$filterMap,
			function (c) {
				if (!c.$) {
					var v = c.a;
					return elm$core$Maybe$Just(v);
				} else {
					return elm$core$Maybe$Nothing;
				}
			},
			defnConstraints);
		var commands = _Utils_ap(
			_List_fromArray(
				['(set-option :produce-models true)']),
			_Utils_ap(
				defns,
				_Utils_ap(
					wordFun,
					_Utils_ap(
						assertions,
						_List_fromArray(
							[
								'(check-sat)',
								'(get-value (' + (A2(elm$core$String$join, ' ', vars) + '))')
							])))));
		return A2(elm$core$String$join, '\n', commands);
	});
var author$project$Solver$generateNumberingProblem = function (puzzle) {
	return elm$json$Json$Encode$string(
		A2(
			author$project$Solver$smt2OfConstraints,
			author$project$Puzzle$quoteIndexWords(puzzle),
			A2(
				author$project$Solver$constraintsOfPuzzle,
				author$project$Puzzle$quoteIndices(puzzle),
				puzzle)));
};
var author$project$Solver$missingResult = {ag: author$project$Solver$SMTFailed, aj: 0};
var author$project$Wordlist$compareEntry = F2(
	function (e1, e2) {
		return A2(elm$core$Basics$compare, e1.D, e2.D);
	});
var author$project$Wordlist$trieInsert = F2(
	function (entry, t1) {
		var _n0 = elm$core$String$toList(entry.D);
		if ((_n0.b && _n0.b.b) && _n0.b.b.b) {
			var c1 = _n0.a;
			var _n1 = _n0.b;
			var c2 = _n1.a;
			var _n2 = _n1.b;
			var c3 = _n2.a;
			var t2 = A2(
				elm$core$Maybe$withDefault,
				elm$core$Dict$empty,
				A2(elm$core$Dict$get, c1, t1));
			var t2Updated = function () {
				var t3 = A2(
					elm$core$Maybe$withDefault,
					elm$core$Dict$empty,
					A2(elm$core$Dict$get, c2, t2));
				var t3Updated = A3(
					elm$core$Dict$update,
					c3,
					function (mstrs) {
						return elm$core$Maybe$Just(
							A3(
								author$project$Util$insertWith,
								author$project$Wordlist$compareEntry,
								entry,
								A2(elm$core$Maybe$withDefault, _List_Nil, mstrs)));
					},
					t3);
				return A3(elm$core$Dict$insert, c2, t3Updated, t2);
			}();
			return A3(elm$core$Dict$insert, c1, t2Updated, t1);
		} else {
			return t1;
		}
	});
var author$project$Wordlist$generateWordlist = F2(
	function (source, words) {
		return A3(
			elm$core$List$foldr,
			author$project$Wordlist$trieInsert,
			author$project$Wordlist$emptyTrie,
			A2(
				elm$core$List$map,
				function (word) {
					return {
						aP: '',
						ap: source,
						S: '',
						D: elm$core$String$toUpper(word)
					};
				},
				words));
	});
var author$project$Wordlist$load = F2(
	function (source, contents) {
		return A2(
			author$project$Wordlist$generateWordlist,
			source,
			elm$core$String$words(contents));
	});
var elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var elm$core$Process$sleep = _Process_sleep;
var elm$core$String$toInt = _String_toInt;
var elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var elm$http$Http$fractionReceived = function (p) {
	var _n0 = p.aD;
	if (_n0.$ === 1) {
		return 0;
	} else {
		var n = _n0.a;
		return (!n) ? 1 : A3(elm$core$Basics$clamp, 0, 1, p.b9 / n);
	}
};
var author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var title = msg.a;
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$asCurrentPuzzleIn,
						model,
						author$project$Puzzle$fixupAnswerInitials(
							A2(author$project$Puzzle$setTitle, title, model.e))));
			case 1:
				var author = msg.a;
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$asCurrentPuzzleIn,
						model,
						author$project$Puzzle$fixupAnswerInitials(
							A2(author$project$Puzzle$setAuthor, author, model.e))));
			case 2:
				var quote = msg.a;
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$asCurrentPuzzleIn,
						model,
						author$project$Puzzle$fixupAnswerInitials(
							A2(author$project$Puzzle$setQuote, quote, model.e))));
			case 3:
				var idx = msg.a;
				var answer = msg.b;
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$asCurrentPuzzleIn,
						model,
						A3(author$project$Puzzle$updateAnswer, idx, answer, model.e)));
			case 4:
				var idx = msg.a;
				return A2(
					author$project$Main$andSave,
					0,
					_Utils_update(
						model,
						{
							t: ((0 <= idx) && ((_Utils_cmp(
								idx,
								elm$core$List$length(model.e.W)) < 0) && (!A2(elm$core$List$member, idx, model.t)))) ? _List_fromArray(
								[idx]) : model.t
						}));
			case 5:
				var idxs = msg.a;
				return A2(
					author$project$Main$andSave,
					0,
					_Utils_update(
						model,
						{
							t: A2(
								elm$core$List$filter,
								function (idx) {
									return (0 <= idx) && (_Utils_cmp(
										idx,
										elm$core$List$length(model.e.W)) < 0);
								},
								idxs)
						}));
			case 6:
				var idx = msg.a;
				var hint = msg.b;
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$asCurrentPuzzleIn,
						model,
						A3(author$project$Puzzle$updateHint, idx, hint, model.e)));
			case 7:
				var idx = msg.a;
				var numIdx = msg.b;
				var newNum = msg.c;
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$asCurrentPuzzleIn,
						model,
						A4(
							author$project$Puzzle$updateNumbering,
							idx,
							numIdx,
							elm$core$String$toInt(newNum),
							model.e)));
			case 8:
				var phase = msg.a;
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$asCurrentPuzzleIn,
						model,
						A2(author$project$Puzzle$setPhase, phase, model.e)));
			case 9:
				if (!msg.a) {
					var _n1 = msg.a;
					var now = msg.b;
					var newModel = A2(
						author$project$Main$asCurrentPuzzleIn,
						model,
						A2(author$project$Puzzle$setTimeModified, now, model.e));
					return _Utils_Tuple2(
						newModel,
						author$project$Main$saveCurrentPuzzle(
							author$project$Puzzle$encodePuzzle(newModel.e)));
				} else {
					var _n2 = msg.a;
					return _Utils_Tuple2(
						model,
						elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									author$project$Main$saveCurrentPuzzle(
									author$project$Puzzle$encodePuzzle(model.e)),
									author$project$Main$savePuzzles(
									A2(elm$json$Json$Encode$list, author$project$Puzzle$encodePuzzle, model.s))
								])));
				}
			case 10:
				return A2(
					author$project$Main$andSave,
					1,
					A2(author$project$Main$popCurrentPuzzle, author$project$Puzzle$emptyPuzzle, model));
			case 11:
				return _Utils_Tuple2(
					A2(author$project$Main$pendingDeletion, true, model),
					elm$core$Platform$Cmd$none);
			case 12:
				if (!msg.a) {
					return _Utils_Tuple2(
						A2(author$project$Main$pendingDeletion, false, model),
						elm$core$Platform$Cmd$none);
				} else {
					return A2(
						author$project$Main$andSave,
						0,
						A2(
							author$project$Main$pendingDeletion,
							false,
							A2(author$project$Main$asCurrentPuzzleIn, model, author$project$Puzzle$emptyPuzzle)));
				}
			case 13:
				var sIndex = msg.a;
				return _Utils_Tuple2(
					A2(
						author$project$Main$asSelectedPuzzleIn,
						model,
						elm$core$List$head(
							function (index) {
								return A2(elm$core$List$drop, index, model.s);
							}(
								A2(
									elm$core$Maybe$withDefault,
									elm$core$List$length(model.s),
									elm$core$String$toInt(sIndex))))),
					elm$core$Platform$Cmd$none);
			case 14:
				var savedPuzzle = msg.a;
				return A2(
					author$project$Main$andSave,
					1,
					A2(
						author$project$Main$loadPuzzle,
						savedPuzzle,
						author$project$Main$clearSelectedPuzzle(model)));
			case 15:
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$withSolverResult,
						elm$core$Maybe$Nothing,
						A2(
							author$project$Main$asCurrentPuzzleIn,
							model,
							author$project$Puzzle$clearNumbering(model.e))));
			case 16:
				return _Utils_Tuple2(
					model,
					author$project$Main$solveNumbering(
						author$project$Solver$generateNumberingProblem(model.e)));
			case 17:
				var json = msg.a;
				return A2(
					author$project$Main$andSave,
					0,
					A2(
						author$project$Main$tryApplySMTNumberingTo,
						model,
						A2(
							elm$core$Result$withDefault,
							author$project$Solver$missingResult,
							A2(elm$json$Json$Decode$decodeValue, author$project$Solver$decodeSMTResult, json))));
			case 18:
				var json = msg.a;
				return _Utils_Tuple2(
					A2(
						author$project$Main$asSolverStateIn,
						model,
						A2(
							elm$core$Result$withDefault,
							0,
							A2(elm$json$Json$Decode$decodeValue, author$project$SMT$decodeSolverState, json))),
					elm$core$Platform$Cmd$none);
			case 19:
				var here = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aq: here}),
					elm$core$Platform$Cmd$none);
			case 20:
				if (msg.b.$ === 1) {
					var wl = msg.a;
					var err = msg.b.a;
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				} else {
					var wl = msg.a;
					var words = msg.b.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								ar: A2(author$project$Wordlist$load, wl.ap, words)
							}),
						elm$core$Platform$Cmd$none);
				}
			case 21:
				if (!msg.b.$) {
					var wl = msg.a;
					var progress = msg.b.a;
					return _Utils_Tuple2(model, elm$core$Platform$Cmd$none);
				} else {
					var s = msg.a;
					var progress = msg.b.a;
					var recvd = elm$http$Http$fractionReceived(progress);
					return _Utils_Tuple2(
						A3(author$project$Main$updateProgress, s, recvd, model),
						(recvd === 1.0) ? A2(
							elm$core$Task$perform,
							function (_n3) {
								return author$project$Main$ClearProgress(s);
							},
							elm$core$Process$sleep(1000)) : elm$core$Platform$Cmd$none);
				}
			default:
				var s = msg.a;
				return _Utils_Tuple2(
					A2(author$project$Main$clearProgress, s, model),
					elm$core$Platform$Cmd$none);
		}
	});
var elm$core$List$sum = function (numbers) {
	return A3(elm$core$List$foldl, elm$core$Basics$add, 0, numbers);
};
var author$project$Hist$count = function (h) {
	return elm$core$List$sum(
		elm$core$Dict$values(h));
};
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
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
var elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _n0) {
				stepState:
				while (true) {
					var list = _n0.a;
					var result = _n0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _n2 = list.a;
						var lKey = _n2.a;
						var lValue = _n2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_n0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_n0 = $temp$_n0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _n3 = A3(
			elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _n3.a;
		var intermediateResult = _n3.b;
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n4, result) {
					var k = _n4.a;
					var v = _n4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var author$project$Hist$difference = F2(
	function (hSub, hSup) {
		return A6(
			elm$core$Dict$merge,
			F3(
				function (c, cSub, d) {
					return A3(elm$core$Dict$insert, c, -cSub, d);
				}),
			F4(
				function (c, cSub, cSup, d) {
					return A3(elm$core$Dict$insert, c, cSup - cSub, d);
				}),
			F3(
				function (c, cSup, d) {
					return A3(elm$core$Dict$insert, c, cSup, d);
				}),
			hSub,
			hSup,
			elm$core$Dict$empty);
	});
var author$project$Hist$empty = elm$core$Dict$empty;
var author$project$Hist$fromString = function (s) {
	var incrementCount = function (mcnt) {
		if (mcnt.$ === 1) {
			return elm$core$Maybe$Just(1);
		} else {
			var cnt = mcnt.a;
			return elm$core$Maybe$Just(cnt + 1);
		}
	};
	return A3(
		elm$core$List$foldl,
		F2(
			function (c, m) {
				return A3(elm$core$Dict$update, c, incrementCount, m);
			}),
		author$project$Hist$empty,
		author$project$Util$cleanChars(s));
};
var author$project$Hist$isEmpty = function (h) {
	return A2(
		elm$core$List$all,
		function (cnt) {
			return !cnt;
		},
		elm$core$Dict$values(h));
};
var elm$core$Dict$filter = F2(
	function (isGood, dict) {
		return A3(
			elm$core$Dict$foldl,
			F3(
				function (k, v, d) {
					return A2(isGood, k, v) ? A3(elm$core$Dict$insert, k, v, d) : d;
				}),
			elm$core$Dict$empty,
			dict);
	});
var elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === -2) {
		return true;
	} else {
		return false;
	}
};
var author$project$Hist$isExhausted = function (h) {
	return elm$core$Dict$isEmpty(
		A2(
			elm$core$Dict$filter,
			F2(
				function (c, cnt) {
					return cnt > 0;
				}),
			h));
};
var author$project$Util$alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var author$project$Util$alphabetList = elm$core$String$toList(author$project$Util$alphabet);
var author$project$Hist$emptyLetter = elm$core$Dict$fromList(
	A2(
		elm$core$List$map,
		function (c) {
			return _Utils_Tuple2(c, 0);
		},
		author$project$Util$alphabetList));
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var author$project$Hist$cleanLetterHist = function (h) {
	return A2(elm$core$Dict$union, h, author$project$Hist$emptyLetter);
};
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(
			A3(elm$core$List$foldl, elm$core$Basics$max, x, xs));
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$String$fromFloat = _String_fromNumber;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var elm$svg$Svg$g = elm$svg$Svg$trustedNode('g');
var elm$svg$Svg$rect = elm$svg$Svg$trustedNode('rect');
var elm$svg$Svg$svg = elm$svg$Svg$trustedNode('svg');
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$svg$Svg$text = elm$virtual_dom$VirtualDom$text;
var elm$svg$Svg$text_ = elm$svg$Svg$trustedNode('text');
var elm$svg$Svg$title = elm$svg$Svg$trustedNode('title');
var elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var elm$svg$Svg$Attributes$dominantBaseline = _VirtualDom_attribute('dominant-baseline');
var elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var elm$svg$Svg$Attributes$id = _VirtualDom_attribute('id');
var elm$svg$Svg$Attributes$textAnchor = _VirtualDom_attribute('text-anchor');
var elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var author$project$Hist$toSVG = F2(
	function (hQuote, hRemaining) {
		var width = 300;
		var hr = author$project$Hist$cleanLetterHist(hRemaining);
		var hq = author$project$Hist$cleanLetterHist(hQuote);
		var height = 120;
		var hooray = (author$project$Hist$isEmpty(hr) && (!author$project$Hist$isEmpty(hq))) ? _List_fromArray(
			[
				A2(
				elm$svg$Svg$text_,
				_List_fromArray(
					[
						elm$svg$Svg$Attributes$x(
						elm$core$String$fromFloat(width / 2)),
						elm$svg$Svg$Attributes$y(
						elm$core$String$fromFloat(height / 2)),
						elm$svg$Svg$Attributes$textAnchor('middle'),
						elm$svg$Svg$Attributes$dominantBaseline('middle'),
						elm$svg$Svg$Attributes$class('hooray')
					]),
				_List_fromArray(
					[
						elm$svg$Svg$text('🎉')
					]))
			]) : _List_Nil;
		var letterY = height - 2;
		var hc = A2(elm$core$Dict$union, hq, hr);
		var maxRemaining = A2(
			elm$core$Maybe$withDefault,
			0,
			elm$core$List$maximum(
				elm$core$Dict$values(hc)));
		var barHeight = function (cnt) {
			return 100 * (cnt / maxRemaining);
		};
		var barBaseY = height - 10;
		var allLetters = elm$core$Dict$keys(hc);
		var letterSpacing = width / elm$core$List$length(allLetters);
		var barWidth = letterSpacing * 0.75;
		var letterStart = letterSpacing / 2;
		var letterX = function (index) {
			return letterStart + (letterSpacing * index);
		};
		var barX = function (index) {
			return letterX(index) - (barWidth / 2);
		};
		var bars = F2(
			function (cls, h) {
				return (maxRemaining > 0) ? A2(
					elm$core$List$indexedMap,
					F2(
						function (index, cnt) {
							var countCls = (cnt >= 0) ? 'valid' : 'invalid';
							var barH = barHeight(cnt);
							var barY = A2(elm$core$Basics$min, barBaseY - barH, barBaseY);
							var counter = A2(
								elm$svg$Svg$text_,
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$x(
										elm$core$String$fromFloat(
											letterX(index))),
										elm$svg$Svg$Attributes$y(
										elm$core$String$fromFloat(barY - 2)),
										elm$svg$Svg$Attributes$textAnchor('middle'),
										elm$svg$Svg$Attributes$class(countCls)
									]),
								_List_fromArray(
									[
										elm$svg$Svg$text(
										elm$core$String$fromInt(cnt))
									]));
							var bar = A2(
								elm$svg$Svg$rect,
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$x(
										elm$core$String$fromFloat(
											barX(index))),
										elm$svg$Svg$Attributes$y(
										elm$core$String$fromFloat(barY)),
										elm$svg$Svg$Attributes$width(
										elm$core$String$fromFloat(barWidth)),
										elm$svg$Svg$Attributes$height(
										elm$core$String$fromFloat(barH))
									]),
								_List_Nil);
							return A2(
								elm$svg$Svg$g,
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$class(cls)
									]),
								_List_fromArray(
									[bar, counter]));
						}),
					elm$core$Dict$values(h)) : _List_Nil;
			});
		var quoteBars = A2(bars, 'quote', hq);
		var remainingBars = A2(bars, 'remaining', hr);
		var letterLabels = A2(
			elm$core$List$indexedMap,
			F2(
				function (index, letter) {
					return A2(
						elm$svg$Svg$text_,
						_List_fromArray(
							[
								elm$svg$Svg$Attributes$x(
								elm$core$String$fromFloat(
									letterX(index))),
								elm$svg$Svg$Attributes$y(
								elm$core$String$fromFloat(letterY)),
								elm$svg$Svg$Attributes$textAnchor('middle'),
								elm$svg$Svg$Attributes$class('label')
							]),
						_List_fromArray(
							[
								elm$svg$Svg$text(
								elm$core$String$fromChar(letter))
							]));
				}),
			allLetters);
		return A2(
			elm$svg$Svg$svg,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$viewBox(
					'0 0 ' + (elm$core$String$fromInt(width) + (' ' + elm$core$String$fromInt(height)))),
					elm$svg$Svg$Attributes$id('remaining')
				]),
			_Utils_ap(
				letterLabels,
				_Utils_ap(
					quoteBars,
					_Utils_ap(
						remainingBars,
						_Utils_ap(
							hooray,
							_List_fromArray(
								[
									A2(
									elm$svg$Svg$title,
									_List_Nil,
									_List_fromArray(
										[
											elm$svg$Svg$text('Letters remaining')
										]))
								]))))));
	});
var elm$core$String$concat = function (strings) {
	return A2(elm$core$String$join, '', strings);
};
var elm$core$Bitwise$and = _Bitwise_and;
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
var author$project$Hist$toShortString = function (h) {
	return elm$core$String$concat(
		A2(
			elm$core$List$map,
			function (_n0) {
				var c = _n0.a;
				var cnt = _n0.b;
				return A2(
					elm$core$String$repeat,
					cnt,
					elm$core$String$fromChar(c));
			},
			elm$core$Dict$toList(
				A2(
					elm$core$Dict$filter,
					F2(
						function (c, cnt) {
							return cnt > 0;
						}),
					h))));
};
var author$project$Main$Answer = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var author$project$Main$Author = function (a) {
	return {$: 1, a: a};
};
var author$project$Main$ClearNumbering = {$: 15};
var author$project$Main$DeletePuzzle = {$: 11};
var author$project$Main$Hint = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var author$project$Main$LoadPuzzle = function (a) {
	return {$: 14, a: a};
};
var author$project$Main$NewPuzzle = {$: 10};
var author$project$Main$Number = F3(
	function (a, b, c) {
		return {$: 7, a: a, b: b, c: c};
	});
var author$project$Main$Phase = function (a) {
	return {$: 8, a: a};
};
var author$project$Main$Quote = function (a) {
	return {$: 2, a: a};
};
var author$project$Main$ReallyDeletePuzzle = function (a) {
	return {$: 12, a: a};
};
var author$project$Main$SelectClue = function (a) {
	return {$: 4, a: a};
};
var author$project$Main$SelectPuzzle = function (a) {
	return {$: 13, a: a};
};
var author$project$Main$SolveNumbering = {$: 16};
var author$project$Main$Title = function (a) {
	return {$: 0, a: a};
};
var author$project$Main$anagramDatalistId = function (letter) {
	return 'clue-anagrams-' + letter;
};
var author$project$Puzzle$clueFor = F2(
	function (index, puzzle) {
		return A2(
			elm$core$Maybe$withDefault,
			author$project$Puzzle$defaultClue(''),
			elm$core$List$head(
				A2(elm$core$List$drop, index, puzzle.W)));
	});
var author$project$Puzzle$lettering = function () {
	var aToZ = A2(elm$core$List$map, elm$core$String$fromChar, author$project$Util$alphabetList);
	var aaToZZ = A2(
		elm$core$List$map,
		elm$core$String$repeat(2),
		aToZ);
	return _Utils_ap(aToZ, aaToZZ);
}();
var author$project$Puzzle$letterFor = function (index) {
	return A2(
		elm$core$Maybe$withDefault,
		'',
		elm$core$List$head(
			A2(elm$core$List$drop, index, author$project$Puzzle$lettering)));
};
var author$project$Util$emptySplitList = {bO: _List_Nil, bQ: _List_Nil, cd: _List_Nil, ce: _List_Nil, ck: _List_Nil};
var author$project$Util$splitList = function (list) {
	var loop = F2(
		function (l, sl) {
			loop:
			while (true) {
				if (!l.b) {
					return sl;
				} else {
					var s = l.a;
					var rest = l.b;
					var _n1 = elm$core$String$length(s);
					switch (_n1) {
						case 0:
							var $temp$l = rest,
								$temp$sl = sl;
							l = $temp$l;
							sl = $temp$sl;
							continue loop;
						case 1:
							var $temp$l = rest,
								$temp$sl = sl;
							l = $temp$l;
							sl = $temp$sl;
							continue loop;
						case 2:
							var $temp$l = rest,
								$temp$sl = sl;
							l = $temp$l;
							sl = $temp$sl;
							continue loop;
						case 3:
							var $temp$l = rest,
								$temp$sl = _Utils_update(
								sl,
								{
									ck: A2(elm$core$List$cons, s, sl.ck)
								});
							l = $temp$l;
							sl = $temp$sl;
							continue loop;
						case 4:
							var $temp$l = rest,
								$temp$sl = _Utils_update(
								sl,
								{
									bQ: A2(elm$core$List$cons, s, sl.bQ)
								});
							l = $temp$l;
							sl = $temp$sl;
							continue loop;
						case 5:
							var $temp$l = rest,
								$temp$sl = _Utils_update(
								sl,
								{
									bO: A2(elm$core$List$cons, s, sl.bO)
								});
							l = $temp$l;
							sl = $temp$sl;
							continue loop;
						case 6:
							var $temp$l = rest,
								$temp$sl = _Utils_update(
								sl,
								{
									ce: A2(elm$core$List$cons, s, sl.ce)
								});
							l = $temp$l;
							sl = $temp$sl;
							continue loop;
						default:
							var $temp$l = rest,
								$temp$sl = _Utils_update(
								sl,
								{
									cd: A2(elm$core$List$cons, s, sl.cd)
								});
							l = $temp$l;
							sl = $temp$sl;
							continue loop;
					}
				}
			}
		});
	return A2(
		loop,
		elm$core$List$reverse(list),
		author$project$Util$emptySplitList);
};
var author$project$Hist$foundIn = F2(
	function (hist, s) {
		return author$project$Hist$isExhausted(
			A2(
				author$project$Hist$difference,
				hist,
				author$project$Hist$fromString(s)));
	});
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var author$project$Wordlist$trieSuffixes = F3(
	function (word, s, t1) {
		if (word.b) {
			if (word.b.b) {
				if (word.b.b.b) {
					var c1 = word.a;
					var _n1 = word.b;
					var c2 = _n1.a;
					var _n2 = _n1.b;
					var c3 = _n2.a;
					return A2(
						elm$core$List$filter,
						A2(
							elm$core$Basics$composeR,
							function ($) {
								return $.D;
							},
							elm$core$String$startsWith(s)),
						A2(
							elm$core$Maybe$withDefault,
							_List_Nil,
							A2(
								elm$core$Maybe$andThen,
								elm$core$Dict$get(c3),
								A2(
									elm$core$Maybe$andThen,
									elm$core$Dict$get(c2),
									A2(elm$core$Dict$get, c1, t1)))));
				} else {
					var c1 = word.a;
					var _n3 = word.b;
					var c2 = _n3.a;
					return elm$core$List$concat(
						elm$core$Dict$values(
							A2(
								elm$core$Maybe$withDefault,
								elm$core$Dict$empty,
								A2(
									elm$core$Maybe$andThen,
									elm$core$Dict$get(c2),
									A2(elm$core$Dict$get, c1, t1)))));
				}
			} else {
				return _List_Nil;
			}
		} else {
			return _List_Nil;
		}
	});
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var author$project$Wordlist$anagramsFor = F3(
	function (wl, remainingHist, prefix) {
		if (!prefix.b) {
			return _List_Nil;
		} else {
			var c = prefix.a;
			var rest = prefix.b;
			var s = elm$core$String$fromList(prefix);
			return A2(
				elm$core$List$filter,
				A2(
					elm$core$Basics$composeR,
					elm$core$String$dropLeft(
						elm$core$String$length(s)),
					author$project$Hist$foundIn(remainingHist)),
				A2(
					elm$core$List$map,
					function ($) {
						return $.D;
					},
					A3(author$project$Wordlist$trieSuffixes, prefix, s, wl)));
		}
	});
var elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$h4 = _VirtualDom_node('h4');
var elm$html$Html$option = _VirtualDom_node('option');
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var elm$html$Html$Attributes$id = elm$html$Html$Attributes$stringProperty('id');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$Keyed$node = elm$virtual_dom$VirtualDom$keyedNode;
var author$project$Main$anagramAssistance = F4(
	function (puzzle, wordlist, remainingHist, index) {
		var letter = author$project$Puzzle$letterFor(index);
		var clue = A2(author$project$Puzzle$clueFor, index, puzzle);
		var prefix = function () {
			var _n0 = A2(elm$core$List$map, elm$core$Tuple$second, clue.ag);
			if (!_n0.b) {
				return A2(
					elm$core$List$map,
					elm$core$Char$toUpper,
					A2(
						elm$core$List$take,
						1,
						A2(
							elm$core$List$drop,
							index,
							elm$core$String$toList(
								author$project$Puzzle$initialism(puzzle)))));
			} else {
				var cs = _n0;
				return cs;
			}
		}();
		var anagrams = A3(author$project$Wordlist$anagramsFor, wordlist, remainingHist, prefix);
		var split = author$project$Util$splitList(anagrams);
		var anagramEntry = F3(
			function (num, descr, l) {
				return A3(
					elm$html$Html$Keyed$node,
					'div',
					_List_fromArray(
						[
							elm$html$Html$Attributes$class(
							'anagram-group' + elm$core$String$fromInt(num))
						]),
					A2(
						elm$core$List$cons,
						_Utils_Tuple2(
							'header' + elm$core$String$fromInt(num),
							A2(
								elm$html$Html$h4,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(descr)
									]))),
						A2(
							elm$core$List$map,
							function (w) {
								return _Utils_Tuple2(
									w,
									A2(
										elm$html$Html$div,
										_List_fromArray(
											[
												elm$html$Html$Attributes$class('anagram')
											]),
										_List_fromArray(
											[
												elm$html$Html$text(w)
											])));
							},
							l)));
			});
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$id('anagram-assistance-' + letter),
					elm$html$Html$Attributes$class('anagrams')
				]),
			_List_fromArray(
				[
					A3(
					elm$html$Html$Keyed$node,
					'datalist',
					_List_fromArray(
						[
							elm$html$Html$Attributes$id(
							author$project$Main$anagramDatalistId(letter))
						]),
					A2(
						elm$core$List$map,
						function (anagram) {
							return _Utils_Tuple2(
								anagram,
								A2(
									elm$html$Html$option,
									_List_fromArray(
										[
											elm$html$Html$Attributes$value(anagram)
										]),
									_List_Nil));
						},
						anagrams)),
					A3(anagramEntry, 3, '3 letters', split.ck),
					A3(anagramEntry, 4, '4 letters', split.bQ),
					A3(anagramEntry, 5, '5 letters', split.bO),
					A3(anagramEntry, 6, '6 letters', split.ce),
					A3(anagramEntry, 7, '7+ letters', split.cd)
				]));
	});
var author$project$Main$baseTabs = 3;
var author$project$Main$SelectClues = function (a) {
	return {$: 5, a: a};
};
var elm$core$Set$Set_elm_builtin = elm$core$Basics$identity;
var elm$core$Set$empty = elm$core$Dict$empty;
var elm$core$Set$insert = F2(
	function (key, _n0) {
		var dict = _n0;
		return A3(elm$core$Dict$insert, key, 0, dict);
	});
var elm$core$Set$fromList = function (list) {
	return A3(elm$core$List$foldl, elm$core$Set$insert, elm$core$Set$empty, list);
};
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Main$boardToSVG = F3(
	function (numCols, qIndexUses, puzzle) {
		var width = 300;
		var quoteWords = A2(
			elm$core$List$map,
			author$project$Util$cleanString,
			A2(
				elm$core$List$filter,
				A2(elm$core$Basics$composeL, elm$core$Basics$not, elm$core$String$isEmpty),
				elm$core$String$words(puzzle.aC)));
		var quoteText = elm$core$String$toList(
			A2(elm$core$String$join, ' ', quoteWords));
		var numberedQuoteText = function () {
			var number = F3(
				function (idx, count, l) {
					var row = (count / numCols) | 0;
					if (!l.b) {
						return A2(
							elm$core$List$map,
							function (col) {
								return {L: ' ', ah: col, P: -1, aa: row};
							},
							A2(elm$core$List$range, count % numCols, numCols));
					} else {
						var c = l.a;
						var rest = l.b;
						var square = {
							L: c,
							ah: count % numCols,
							P: (c === ' ') ? (-1) : idx,
							aa: row
						};
						return A2(
							elm$core$List$cons,
							square,
							A3(
								number,
								idx + ((c === ' ') ? 0 : 1),
								count + 1,
								rest));
					}
				});
			return A3(number, 0, 0, quoteText);
		}();
		var numBoxes = elm$core$List$length(quoteText);
		var numRows = ((numBoxes / numCols) | 0) + ((!(numBoxes % numCols)) ? 0 : 1);
		var boxWidth = width / numCols;
		var height = numRows * boxWidth;
		var quoteRows = A2(
			elm$core$List$map,
			function (square) {
				var y = square.aa * boxWidth;
				var x = square.ah * boxWidth;
				var usedIn = elm$core$Set$toList(
					elm$core$Set$fromList(
						A2(
							elm$core$List$map,
							elm$core$Tuple$first,
							A2(
								elm$core$Maybe$withDefault,
								_List_Nil,
								A2(elm$core$Dict$get, square.P, qIndexUses)))));
				var thirdBox = boxWidth / 3;
				var textLength = elm$core$String$fromFloat(thirdBox);
				return A2(
					elm$svg$Svg$g,
					_List_fromArray(
						[
							elm$svg$Svg$Attributes$class(
							'row-' + elm$core$String$fromInt(square.aa)),
							elm$svg$Svg$Attributes$class(
							'qIndex-' + elm$core$String$fromInt(square.P)),
							elm$html$Html$Events$onClick(
							author$project$Main$SelectClues(usedIn))
						]),
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								elm$svg$Svg$rect,
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$x(
										elm$core$String$fromFloat(x)),
										elm$svg$Svg$Attributes$y(
										elm$core$String$fromFloat(y)),
										elm$svg$Svg$Attributes$width(
										elm$core$String$fromFloat(boxWidth)),
										elm$svg$Svg$Attributes$height(
										elm$core$String$fromFloat(boxWidth)),
										elm$svg$Svg$Attributes$class('board-square'),
										elm$svg$Svg$Attributes$class(
										(square.L === ' ') ? 'board-space' : 'board-letter'),
										elm$svg$Svg$Attributes$class(
										(elm$core$List$length(usedIn) > 1) ? 'board-number-conflict' : '')
									]),
								_List_Nil)
							]),
						(square.L === ' ') ? _List_Nil : _List_fromArray(
							[
								A2(
								elm$svg$Svg$text_,
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$x(
										elm$core$String$fromFloat(x + 1)),
										elm$svg$Svg$Attributes$y(
										elm$core$String$fromFloat(y + thirdBox)),
										elm$svg$Svg$Attributes$textAnchor('start'),
										elm$svg$Svg$Attributes$class('number')
									]),
								_List_fromArray(
									[
										elm$svg$Svg$text(
										elm$core$String$fromInt(square.P + 1))
									])),
								A2(
								elm$svg$Svg$text_,
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$x(
										elm$core$String$fromFloat((x + boxWidth) - 1)),
										elm$svg$Svg$Attributes$y(
										elm$core$String$fromFloat(y + thirdBox)),
										elm$svg$Svg$Attributes$textAnchor('end'),
										elm$svg$Svg$Attributes$class('clue-letter')
									]),
								_List_fromArray(
									[
										elm$svg$Svg$text(
										elm$core$String$concat(
											A2(elm$core$List$map, author$project$Puzzle$letterFor, usedIn)))
									])),
								A2(
								elm$svg$Svg$text_,
								_List_fromArray(
									[
										elm$svg$Svg$Attributes$x(
										elm$core$String$fromFloat(x + (boxWidth / 2))),
										elm$svg$Svg$Attributes$y(
										elm$core$String$fromFloat((y + boxWidth) - 2)),
										elm$svg$Svg$Attributes$textAnchor('middle'),
										elm$svg$Svg$Attributes$class('letter')
									]),
								_List_fromArray(
									[
										elm$svg$Svg$text(
										elm$core$String$fromChar(square.L))
									]))
							])));
			},
			numberedQuoteText);
		return A2(
			elm$svg$Svg$svg,
			_List_fromArray(
				[
					elm$svg$Svg$Attributes$viewBox(
					'0 0 ' + (elm$core$String$fromFloat(width) + (' ' + elm$core$String$fromFloat(height)))),
					elm$html$Html$Attributes$id('board')
				]),
			quoteRows);
	});
var elm$html$Html$input = _VirtualDom_node('input');
var elm$html$Html$Attributes$placeholder = elm$html$Html$Attributes$stringProperty('placeholder');
var elm$html$Html$Attributes$type_ = elm$html$Html$Attributes$stringProperty('type');
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
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
var author$project$Main$textInput = F4(
	function (attrs, p, v, toMsg) {
		return A2(
			elm$html$Html$input,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Attributes$type_('text'),
						elm$html$Html$Attributes$placeholder(p),
						elm$html$Html$Attributes$value(v),
						elm$html$Html$Events$onInput(toMsg)
					]),
				attrs),
			_List_Nil);
	});
var author$project$Puzzle$addIndex = function (l) {
	return A2(elm$core$List$indexedMap, elm$core$Tuple$pair, l);
};
var author$project$Puzzle$addInitials = F2(
	function (initial, clues) {
		return A3(elm$core$List$map2, elm$core$Tuple$pair, initial, clues);
	});
var author$project$Puzzle$phases = _List_fromArray(
	[0, 1, 2]);
var author$project$Puzzle$shortPuzzleDescription = function (puzzle) {
	return elm$core$String$toUpper(puzzle.aJ) + (' — ' + elm$core$String$toUpper(puzzle.bv));
};
var elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)),
			string);
	});
var author$project$Util$twoDigits = function (i) {
	return A3(
		elm$core$String$padLeft,
		2,
		'0',
		elm$core$String$fromInt(i));
};
var elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return elm$core$Basics$floor(numerator / denominator);
	});
var elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.aE, posixMinutes) < 0) {
					return posixMinutes + era.b;
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
		aN: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		a4: month,
		by: year + ((month <= 2) ? 1 : 0)
	};
};
var elm$time$Time$toDay = F2(
	function (zone, time) {
		return elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).aN;
	});
var elm$time$Time$Apr = 3;
var elm$time$Time$Aug = 7;
var elm$time$Time$Dec = 11;
var elm$time$Time$Feb = 1;
var elm$time$Time$Jan = 0;
var elm$time$Time$Jul = 6;
var elm$time$Time$Jun = 5;
var elm$time$Time$Mar = 2;
var elm$time$Time$May = 4;
var elm$time$Time$Nov = 10;
var elm$time$Time$Oct = 9;
var elm$time$Time$Sep = 8;
var elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _n0 = elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).a4;
		switch (_n0) {
			case 1:
				return 0;
			case 2:
				return 1;
			case 3:
				return 2;
			case 4:
				return 3;
			case 5:
				return 4;
			case 6:
				return 5;
			case 7:
				return 6;
			case 8:
				return 7;
			case 9:
				return 8;
			case 10:
				return 9;
			case 11:
				return 10;
			default:
				return 11;
		}
	});
var elm$time$Time$toYear = F2(
	function (zone, time) {
		return elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).by;
	});
var author$project$Util$iso8601Date = F2(
	function (here, now) {
		var yyyy = elm$core$String$fromInt(
			A2(elm$time$Time$toYear, here, now));
		var mm = function () {
			var _n0 = A2(elm$time$Time$toMonth, here, now);
			switch (_n0) {
				case 0:
					return '01';
				case 1:
					return '02';
				case 2:
					return '03';
				case 3:
					return '04';
				case 4:
					return '05';
				case 5:
					return '06';
				case 6:
					return '07';
				case 7:
					return '08';
				case 8:
					return '09';
				case 9:
					return '10';
				case 10:
					return '11';
				default:
					return '12';
			}
		}();
		var dd = author$project$Util$twoDigits(
			A2(elm$time$Time$toDay, here, now));
		return A2(
			elm$core$String$join,
			'-',
			_List_fromArray(
				[yyyy, mm, dd]));
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
var elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			elm$core$Basics$modBy,
			60,
			A2(elm$time$Time$toAdjustedMinutes, zone, time));
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
var author$project$Util$iso8601Time = F2(
	function (here, now) {
		var ss = author$project$Util$twoDigits(
			A2(elm$time$Time$toSecond, here, now));
		var mm = author$project$Util$twoDigits(
			A2(elm$time$Time$toMinute, here, now));
		var hh = author$project$Util$twoDigits(
			A2(elm$time$Time$toHour, here, now));
		return A2(
			elm$core$String$join,
			':',
			_List_fromArray(
				[hh, mm, ss]));
	});
var author$project$Util$iso8601DateTime = F2(
	function (here, now) {
		return A2(author$project$Util$iso8601Date, here, now) + (' ' + A2(author$project$Util$iso8601Time, here, now));
	});
var author$project$Puzzle$puzzleDescription = F2(
	function (here, puzzle) {
		return author$project$Puzzle$shortPuzzleDescription(puzzle) + (' (' + (A2(author$project$Util$iso8601DateTime, here, puzzle.B) + ')'));
	});
var author$project$Util$updateAppend = F3(
	function (k, v, d) {
		return A3(
			elm$core$Dict$update,
			k,
			function (mvs) {
				if (mvs.$ === 1) {
					return elm$core$Maybe$Just(v);
				} else {
					var vs = mvs.a;
					return elm$core$Maybe$Just(
						_Utils_ap(v, vs));
				}
			},
			d);
	});
var author$project$Util$mergeCons = F2(
	function (d1, d2) {
		return A3(
			elm$core$Dict$foldr,
			F3(
				function (k, vs, d) {
					return A3(author$project$Util$updateAppend, k, vs, d);
				}),
			d2,
			d1);
	});
var author$project$Util$mergeConsMany = function (l) {
	if (!l.b) {
		return elm$core$Dict$empty;
	} else {
		if (!l.b.b) {
			var d = l.a;
			return d;
		} else {
			var d = l.a;
			var ds = l.b;
			return A2(
				author$project$Util$mergeCons,
				d,
				author$project$Util$mergeConsMany(ds));
		}
	}
};
var author$project$Puzzle$quoteIndexUses = function (puzzle) {
	return author$project$Util$mergeConsMany(
		A2(
			elm$core$List$indexedMap,
			F2(
				function (i, clue) {
					return A3(
						elm$core$List$foldr,
						F2(
							function (_n0, d) {
								var numIndex = _n0.a;
								var _n1 = _n0.b;
								var mNum = _n1.a;
								if (mNum.$ === 1) {
									return d;
								} else {
									var num = mNum.a;
									return A3(
										author$project$Util$updateCons,
										num,
										_Utils_Tuple2(i, numIndex),
										d);
								}
							}),
						elm$core$Dict$empty,
						A2(elm$core$List$indexedMap, elm$core$Tuple$pair, clue.ag));
				}),
			puzzle.W));
};
var author$project$Puzzle$stringOfPhase = function (p) {
	switch (p) {
		case 0:
			return 'Quote entry';
		case 1:
			return 'Anagramming';
		default:
			return 'Cluing and lettering';
	}
};
var elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						elm$core$List$cons,
						sep,
						A2(elm$core$List$cons, x, rest));
				});
			var spersed = A3(elm$core$List$foldr, step, _List_Nil, tl);
			return A2(elm$core$List$cons, hd, spersed);
		}
	});
var elm$core$List$sortBy = _List_sortBy;
var elm$core$List$sort = function (xs) {
	return A2(elm$core$List$sortBy, elm$core$Basics$identity, xs);
};
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var elm$html$Html$h3 = _VirtualDom_node('h3');
var elm$html$Html$label = _VirtualDom_node('label');
var elm$html$Html$meter = _VirtualDom_node('meter');
var elm$html$Html$section = _VirtualDom_node('section');
var elm$html$Html$select = _VirtualDom_node('select');
var elm$html$Html$span = _VirtualDom_node('span');
var elm$html$Html$table = _VirtualDom_node('table');
var elm$html$Html$td = _VirtualDom_node('td');
var elm$html$Html$textarea = _VirtualDom_node('textarea');
var elm$html$Html$tr = _VirtualDom_node('tr');
var elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var elm$html$Html$Attributes$attribute = elm$virtual_dom$VirtualDom$attribute;
var elm$html$Html$Attributes$cols = function (n) {
	return A2(
		_VirtualDom_attribute,
		'cols',
		elm$core$String$fromInt(n));
};
var elm$json$Json$Encode$bool = _Json_wrap;
var elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$bool(bool));
	});
var elm$html$Html$Attributes$disabled = elm$html$Html$Attributes$boolProperty('disabled');
var elm$html$Html$Attributes$for = elm$html$Html$Attributes$stringProperty('htmlFor');
var elm$html$Html$Attributes$list = _VirtualDom_attribute('list');
var elm$html$Html$Attributes$max = elm$html$Html$Attributes$stringProperty('max');
var elm$html$Html$Attributes$min = elm$html$Html$Attributes$stringProperty('min');
var elm$html$Html$Attributes$name = elm$html$Html$Attributes$stringProperty('name');
var elm$html$Html$Attributes$readonly = elm$html$Html$Attributes$boolProperty('readOnly');
var elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Attributes$selected = elm$html$Html$Attributes$boolProperty('selected');
var elm$html$Html$Attributes$size = function (n) {
	return A2(
		_VirtualDom_attribute,
		'size',
		elm$core$String$fromInt(n));
};
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		elm$core$String$fromInt(n));
};
var elm$html$Html$Events$onFocus = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'focus',
		elm$json$Json$Decode$succeed(msg));
};
var elm$virtual_dom$VirtualDom$lazy4 = _VirtualDom_lazy4;
var elm$html$Html$Lazy$lazy4 = elm$virtual_dom$VirtualDom$lazy4;
var author$project$Main$view = function (model) {
	var puzzle = model.e;
	var qIndexUses = author$project$Puzzle$quoteIndexUses(puzzle);
	var qIndexWords = author$project$Puzzle$quoteIndexWords(puzzle);
	var qIndices = author$project$Puzzle$quoteIndices(puzzle);
	var quoteFixed = puzzle.H;
	var quoteHist = author$project$Hist$fromString(puzzle.aC);
	var initials = author$project$Puzzle$initialism(puzzle);
	var initialismHist = author$project$Hist$fromString(initials);
	var missingHist = A2(author$project$Hist$difference, quoteHist, initialismHist);
	var viable = author$project$Hist$isExhausted(missingHist);
	var clueHist = author$project$Hist$fromString(
		elm$core$String$concat(
			A2(elm$core$List$map, author$project$Puzzle$clueAnswer, puzzle.W)));
	var remainingHist = A2(author$project$Hist$difference, clueHist, quoteHist);
	var readyForPhase = function (phase) {
		return _Utils_eq(puzzle.H, phase) || function () {
			switch (phase) {
				case 0:
					return true;
				case 1:
					return viable && (!author$project$Hist$isEmpty(quoteHist));
				default:
					return viable && ((!author$project$Hist$isEmpty(quoteHist)) && author$project$Hist$isEmpty(remainingHist));
			}
		}();
	};
	var answersFixed = puzzle.H !== 1;
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$id('crossbars-wrapper')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('overview')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$h3,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('header')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Crossbars — Acrostic Constructor')
							])),
						A2(
						elm$html$Html$div,
						_List_Nil,
						A2(
							elm$core$List$intersperse,
							A2(
								elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(' → ')
									])),
							A2(
								elm$core$List$map,
								function (p) {
									return A2(
										elm$html$Html$input,
										_List_fromArray(
											[
												elm$html$Html$Attributes$type_('button'),
												elm$html$Html$Attributes$class('phase'),
												elm$html$Html$Attributes$class(
												_Utils_eq(p, puzzle.H) ? 'active' : 'inactive'),
												elm$html$Html$Attributes$disabled(
												!readyForPhase(p)),
												elm$html$Html$Attributes$value(
												author$project$Puzzle$stringOfPhase(p)),
												elm$html$Html$Events$onClick(
												author$project$Main$Phase(p))
											]),
										_List_Nil);
								},
								author$project$Puzzle$phases)))
					])),
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('saved')
					]),
				_Utils_ap(
					_List_fromArray(
						[
							A2(
							elm$html$Html$h3,
							_List_fromArray(
								[
									elm$html$Html$Attributes$class('header')
								]),
							_List_fromArray(
								[
									elm$html$Html$text('Manage puzzles')
								])),
							A2(
							elm$html$Html$div,
							_List_fromArray(
								[
									elm$html$Html$Attributes$id('current-puzzle')
								]),
							_Utils_ap(
								_List_fromArray(
									[
										A2(
										elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												elm$html$Html$text('Current puzzle: '),
												elm$html$Html$text(
												author$project$Puzzle$shortPuzzleDescription(model.e))
											]))
									]),
								model._ ? _List_Nil : _List_fromArray(
									[
										A2(
										elm$html$Html$input,
										_List_fromArray(
											[
												elm$html$Html$Attributes$type_('button'),
												elm$html$Html$Events$onClick(author$project$Main$DeletePuzzle),
												elm$html$Html$Attributes$value('Delete current puzzle')
											]),
										_List_Nil)
									])))
						]),
					_Utils_ap(
						model._ ? _List_fromArray(
							[
								A2(
								elm$html$Html$div,
								_List_fromArray(
									[
										elm$html$Html$Attributes$id('deletion-prompt')
									]),
								_List_fromArray(
									[
										A2(
										elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												elm$html$Html$text('Are you sure? Deleted puzzles are gone forever.')
											])),
										A2(
										elm$html$Html$input,
										_List_fromArray(
											[
												elm$html$Html$Attributes$type_('button'),
												elm$html$Html$Events$onClick(
												author$project$Main$ReallyDeletePuzzle(false)),
												elm$html$Html$Attributes$value('Cancel')
											]),
										_List_Nil),
										A2(
										elm$html$Html$input,
										_List_fromArray(
											[
												elm$html$Html$Attributes$type_('button'),
												elm$html$Html$Events$onClick(
												author$project$Main$ReallyDeletePuzzle(true)),
												elm$html$Html$Attributes$value('Delete current puzzle')
											]),
										_List_Nil)
									]))
							]) : _List_Nil,
						_List_fromArray(
							[
								A2(
								elm$html$Html$div,
								_List_fromArray(
									[
										elm$html$Html$Attributes$id('saved-puzzles')
									]),
								_Utils_ap(
									_List_fromArray(
										[
											A2(
											elm$html$Html$input,
											_List_fromArray(
												[
													elm$html$Html$Attributes$id('new-puzzle'),
													elm$html$Html$Attributes$type_('button'),
													elm$html$Html$Events$onClick(author$project$Main$NewPuzzle),
													elm$html$Html$Attributes$value('New puzzle')
												]),
											_List_Nil)
										]),
									_List_fromArray(
										[
											A2(
											elm$html$Html$select,
											_List_fromArray(
												[
													elm$html$Html$Attributes$id('saved-puzzle-list'),
													elm$html$Html$Events$onInput(author$project$Main$SelectPuzzle)
												]),
											A2(
												elm$core$List$cons,
												A2(
													elm$html$Html$option,
													_List_fromArray(
														[
															elm$html$Html$Attributes$value(''),
															elm$html$Html$Attributes$selected(
															_Utils_eq(model.I, elm$core$Maybe$Nothing))
														]),
													_List_fromArray(
														[
															elm$html$Html$text('Select saved puzzle...')
														])),
												A2(
													elm$core$List$indexedMap,
													F2(
														function (index, savedPuzzle) {
															return A2(
																elm$html$Html$option,
																_List_fromArray(
																	[
																		elm$html$Html$Attributes$value(
																		elm$core$String$fromInt(index)),
																		elm$html$Html$Attributes$selected(
																		_Utils_eq(
																			model.I,
																			elm$core$Maybe$Just(savedPuzzle)))
																	]),
																_List_fromArray(
																	[
																		elm$html$Html$text(
																		A2(author$project$Puzzle$puzzleDescription, model.aq, savedPuzzle))
																	]));
														}),
													model.s))),
											A2(
											elm$html$Html$input,
											_Utils_ap(
												_List_fromArray(
													[
														elm$html$Html$Attributes$type_('button'),
														elm$html$Html$Attributes$value('Load puzzle')
													]),
												function () {
													var _n0 = model.I;
													if (_n0.$ === 1) {
														return _List_Nil;
													} else {
														var sPuz = _n0.a;
														return _List_fromArray(
															[
																elm$html$Html$Events$onClick(
																author$project$Main$LoadPuzzle(sPuz))
															]);
													}
												}()),
											_List_Nil)
										])))
							])))),
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('quote')
					]),
				quoteFixed ? _List_fromArray(
					[
						A3(author$project$Main$boardToSVG, 24, qIndexUses, model.e)
					]) : _List_fromArray(
					[
						A4(
						author$project$Main$textInput,
						_List_fromArray(
							[
								elm$html$Html$Attributes$tabindex(1),
								elm$html$Html$Attributes$size(60),
								elm$html$Html$Attributes$readonly(quoteFixed)
							]),
						'Title',
						puzzle.bv,
						author$project$Main$Title),
						A4(
						author$project$Main$textInput,
						_List_fromArray(
							[
								elm$html$Html$Attributes$tabindex(2),
								elm$html$Html$Attributes$size(60),
								elm$html$Html$Attributes$readonly(quoteFixed)
							]),
						'Author',
						puzzle.aJ,
						author$project$Main$Author),
						A2(
						elm$html$Html$textarea,
						_List_fromArray(
							[
								elm$html$Html$Attributes$tabindex(3),
								elm$html$Html$Attributes$readonly(quoteFixed),
								elm$html$Html$Attributes$placeholder('Quote'),
								elm$html$Html$Events$onInput(author$project$Main$Quote),
								elm$html$Html$Attributes$rows(6),
								elm$html$Html$Attributes$cols(60),
								A2(elm$html$Html$Attributes$attribute, 'autocapitalize', 'character'),
								elm$html$Html$Attributes$value(puzzle.aC)
							]),
						_List_Nil),
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$id('summary')
							]),
						_List_fromArray(
							[
								A2(
								elm$html$Html$span,
								_List_fromArray(
									[
										elm$html$Html$Attributes$id('viability')
									]),
								_List_fromArray(
									[
										viable ? elm$html$Html$text('Quote has all of the initialism\'s letters') : elm$html$Html$text(
										'The quote does not have some letters the initialism needs: ' + author$project$Hist$toShortString(missingHist))
									])),
								A2(
								elm$html$Html$span,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class('count')
									]),
								_List_fromArray(
									[
										elm$html$Html$text('Total letters: '),
										elm$html$Html$text(
										elm$core$String$fromInt(
											author$project$Hist$count(quoteHist)))
									])),
								A2(
								elm$html$Html$span,
								_List_fromArray(
									[
										elm$html$Html$Attributes$class('count')
									]),
								_List_fromArray(
									[
										elm$html$Html$text('Remaining letters: '),
										elm$html$Html$text(
										elm$core$String$fromInt(
											author$project$Hist$count(remainingHist)))
									]))
							]))
					])),
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('detail')
					]),
				(puzzle.H === 2) ? _List_fromArray(
					[
						A2(
						elm$html$Html$h3,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('header')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Numbering solver')
							])),
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$id('solver-state')
							]),
						_List_fromArray(
							[
								elm$html$Html$text(
								function () {
									var _n1 = model.ao;
									switch (_n1) {
										case 0:
											return 'Numbering solver not loaded';
										case 1:
											return 'Downloading numbering solver code...';
										case 2:
											return 'Initializing numbering solver...';
										case 3:
											return 'Numbering solver ready';
										default:
											return 'Numbering solver running...';
									}
								}())
							])),
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$id('solver-result')
							]),
						_List_fromArray(
							[
								elm$html$Html$text(
								function () {
									var _n2 = model.an;
									if (_n2.$ === 1) {
										return '';
									} else {
										var result = _n2.a;
										var time = function () {
											if (result.aj >= 1000) {
												var ss = elm$core$String$fromInt((result.aj / 1000) | 0);
												var ms = A3(
													elm$core$String$padLeft,
													3,
													'0',
													elm$core$String$fromInt(
														A2(elm$core$Basics$modBy, 1000, result.aj)));
												return ss + ('.' + (ms + 's'));
											} else {
												return elm$core$String$fromInt(result.aj) + 'ms';
											}
										}();
										var _n3 = result.ag;
										switch (_n3.$) {
											case 2:
												return 'Could not find a numbering (' + (time + '). 😦');
											case 1:
												return 'Timed out (' + (time + '). ⏲');
											default:
												return 'Success! 🎊 The puzzle has been automatically numbered in ' + (time + '.');
										}
									}
								}())
							])),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('button'),
								elm$html$Html$Events$onClick(author$project$Main$ClearNumbering),
								elm$html$Html$Attributes$value('Clear numbering')
							]),
						_List_Nil),
						A2(
						elm$html$Html$input,
						_List_fromArray(
							[
								elm$html$Html$Attributes$type_('button'),
								elm$html$Html$Events$onClick(author$project$Main$SolveNumbering),
								elm$html$Html$Attributes$value('Automatically assign numbers')
							]),
						_List_Nil)
					]) : _List_fromArray(
					[
						A2(
						elm$html$Html$h3,
						_List_fromArray(
							[
								elm$html$Html$Attributes$class('header')
							]),
						_List_fromArray(
							[
								elm$html$Html$text('Letters remaining')
							])),
						A2(author$project$Hist$toSVG, quoteHist, remainingHist)
					])),
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('clues')
					]),
				A2(
					elm$core$List$map,
					function (_n4) {
						var index = _n4.a;
						var _n5 = _n4.b;
						var initial = _n5.a;
						var clue = _n5.b;
						var selectedCls = (quoteFixed && A2(elm$core$List$member, index, model.t)) ? _List_fromArray(
							[
								elm$html$Html$Attributes$class('selected')
							]) : _List_Nil;
						var letter = author$project$Puzzle$letterFor(index);
						var lbl = 'clue-' + letter;
						var initialStr = elm$core$String$fromChar(initial);
						var answer = author$project$Puzzle$clueAnswer(clue);
						var validCls = elm$html$Html$Attributes$class(
							A2(
								elm$core$String$startsWith,
								elm$core$String$toUpper(initialStr),
								elm$core$String$toUpper(answer)) ? 'valid' : 'invalid');
						return A2(
							elm$html$Html$div,
							_Utils_ap(
								_List_fromArray(
									[
										elm$html$Html$Events$onClick(
										author$project$Main$SelectClue(index))
									]),
								selectedCls),
							_List_fromArray(
								[
									A2(
									elm$html$Html$label,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('clue-letter'),
											elm$html$Html$Attributes$for(lbl)
										]),
									_List_fromArray(
										[
											elm$html$Html$text(letter + '. ')
										])),
									A4(
									author$project$Main$textInput,
									_List_fromArray(
										[
											elm$html$Html$Attributes$tabindex(index + author$project$Main$baseTabs),
											elm$html$Html$Attributes$name(lbl),
											validCls,
											elm$html$Html$Events$onFocus(
											author$project$Main$SelectClue(index)),
											elm$html$Html$Events$onClick(
											author$project$Main$SelectClue(index)),
											elm$html$Html$Attributes$readonly(answersFixed),
											elm$html$Html$Attributes$list(
											author$project$Main$anagramDatalistId(letter))
										]),
									initialStr + '...',
									clue.cj,
									author$project$Main$Answer(index))
								]));
					},
					author$project$Puzzle$addIndex(
						A2(
							author$project$Puzzle$addInitials,
							elm$core$String$toList(initials),
							puzzle.W)))),
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('clue-info')
					]),
				function () {
					var _n6 = puzzle.H;
					switch (_n6) {
						case 0:
							return _List_Nil;
						case 1:
							return _List_fromArray(
								[
									A2(
									elm$html$Html$h3,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('header')
										]),
									_List_fromArray(
										[
											elm$html$Html$text('Anagrams')
										])),
									elm$core$List$isEmpty(model.t) ? A2(
									elm$html$Html$div,
									_List_fromArray(
										[
											elm$html$Html$Attributes$class('explanatory')
										]),
									_List_fromArray(
										[
											elm$html$Html$text('Select a clue to receive anagram suggestions.')
										])) : A2(
									elm$html$Html$div,
									_List_Nil,
									A2(
										elm$core$List$cons,
										A2(
											elm$html$Html$div,
											_List_fromArray(
												[
													elm$html$Html$Attributes$class('explanatory')
												]),
											_List_fromArray(
												[
													elm$html$Html$text('Type two or more characters to find matching suffixes.')
												])),
										A2(
											elm$core$List$map,
											A4(elm$html$Html$Lazy$lazy4, author$project$Main$anagramAssistance, puzzle, model.ar, remainingHist),
											model.t)))
								]);
						default:
							return A2(
								elm$core$List$map,
								function (index) {
									var numberingFor = F3(
										function (numIndex, mNum, c) {
											return A2(
												elm$html$Html$select,
												_List_fromArray(
													[
														elm$html$Html$Attributes$id(
														'clue-numbering-' + (elm$core$String$fromInt(index) + ('-' + elm$core$String$fromInt(numIndex)))),
														elm$html$Html$Events$onInput(
														A2(author$project$Main$Number, index, numIndex))
													]),
												_Utils_ap(
													_List_fromArray(
														[
															A2(
															elm$html$Html$option,
															_List_fromArray(
																[
																	elm$html$Html$Attributes$value(''),
																	elm$html$Html$Attributes$selected(
																	_Utils_eq(mNum, elm$core$Maybe$Nothing))
																]),
															_List_fromArray(
																[
																	elm$html$Html$text('###')
																]))
														]),
													A2(
														elm$core$List$map,
														function (qIndex) {
															var uses = A2(
																elm$core$List$filter,
																function (_n16) {
																	var uIdx = _n16.a;
																	var uNumIdx = _n16.b;
																	return (!_Utils_eq(uIdx, index)) || (_Utils_eq(uIdx, index) && (!_Utils_eq(uNumIdx, numIndex)));
																},
																A2(
																	elm$core$Maybe$withDefault,
																	_List_Nil,
																	A2(elm$core$Dict$get, qIndex, qIndexUses)));
															var clueMention = function (_n15) {
																var cIdx = _n15.a;
																var cNumIdx = _n15.b;
																return author$project$Puzzle$letterFor(cIdx) + ('. ' + elm$core$String$fromInt(cNumIdx + 1));
															};
															var useText = elm$core$List$isEmpty(uses) ? '' : (' (used by ' + (A2(
																elm$core$String$join,
																', ',
																A2(elm$core$List$map, clueMention, uses)) + ')'));
															return A2(
																elm$html$Html$option,
																_List_fromArray(
																	[
																		elm$html$Html$Attributes$value(
																		elm$core$String$fromInt(qIndex)),
																		elm$html$Html$Attributes$selected(
																		_Utils_eq(
																			mNum,
																			elm$core$Maybe$Just(qIndex)))
																	]),
																_List_fromArray(
																	[
																		elm$html$Html$text(
																		_Utils_ap(
																			elm$core$String$fromInt(qIndex + 1),
																			useText))
																	]));
														},
														A2(
															elm$core$Maybe$withDefault,
															_List_Nil,
															A2(elm$core$Dict$get, c, qIndices)))));
										});
									var clueLetter = author$project$Puzzle$letterFor(index);
									var clue = A2(author$project$Puzzle$clueFor, index, puzzle);
									var answer = clue.ag;
									var clueNumbers = A2(
										elm$core$List$filterMap,
										function (_n13) {
											var ansIndex = _n13.a;
											var _n14 = _n13.b;
											var mNumIndex = _n14.a;
											return A2(
												elm$core$Maybe$map,
												elm$core$Tuple$pair(ansIndex),
												mNumIndex);
										},
										A2(elm$core$List$indexedMap, elm$core$Tuple$pair, answer));
									var clueWords = A2(
										elm$core$List$filterMap,
										function (_n12) {
											var ansIndex = _n12.a;
											var numIndex = _n12.b;
											return A2(
												elm$core$Maybe$map,
												elm$core$Tuple$pair(ansIndex),
												A2(elm$core$Dict$get, numIndex, qIndexWords));
										},
										clueNumbers);
									var dupWords = A2(
										elm$core$List$filter,
										function (_n10) {
											var ansIndex = _n10.a;
											var wIndex = _n10.b;
											return A2(
												elm$core$List$any,
												function (_n11) {
													var otherAnsIndex = _n11.a;
													var otherWIndex = _n11.b;
													return (!_Utils_eq(ansIndex, otherAnsIndex)) && _Utils_eq(wIndex, otherWIndex);
												},
												clueWords);
										},
										clueWords);
									var dupLetters = A2(elm$core$List$map, elm$core$Tuple$first, dupWords);
									var unindexedClueNumbers = A2(elm$core$List$map, elm$core$Tuple$second, clueNumbers);
									var fullyNumbered = _Utils_eq(
										A2(
											elm$core$List$map,
											A2(elm$core$Basics$composeR, elm$core$Tuple$second, elm$core$Maybe$Just),
											clueNumbers),
										A2(elm$core$List$map, elm$core$Tuple$first, answer));
									return A2(
										elm$html$Html$div,
										_List_fromArray(
											[
												elm$html$Html$Attributes$class('clue-detail')
											]),
										_List_fromArray(
											[
												A2(
												elm$html$Html$h3,
												_List_Nil,
												_List_fromArray(
													[
														elm$html$Html$text(clueLetter + '. ')
													])),
												A4(
												author$project$Main$textInput,
												_List_fromArray(
													[
														elm$html$Html$Attributes$tabindex(
														(author$project$Main$baseTabs + elm$core$List$length(puzzle.W)) + 1),
														elm$html$Html$Attributes$class('clue-hint'),
														elm$html$Html$Attributes$value(clue.aY)
													]),
												'Clue hint text',
												clue.aY,
												author$project$Main$Hint(index)),
												A2(
												elm$html$Html$table,
												_List_fromArray(
													[
														elm$html$Html$Attributes$class('clue-numbering')
													]),
												_List_fromArray(
													[
														A2(
														elm$html$Html$tr,
														_List_Nil,
														A2(
															elm$core$List$indexedMap,
															F2(
																function (numIndex, _n7) {
																	var c = _n7.b;
																	var dupClasses = A2(elm$core$List$member, numIndex, dupLetters) ? _List_fromArray(
																		[
																			elm$html$Html$Attributes$class('double-dipped')
																		]) : _List_Nil;
																	return A2(
																		elm$html$Html$td,
																		_Utils_ap(
																			_List_fromArray(
																				[
																					elm$html$Html$Attributes$class('clue-numbering-letter')
																				]),
																			dupClasses),
																		_List_fromArray(
																			[
																				elm$html$Html$text(
																				elm$core$String$fromChar(c))
																			]));
																}),
															answer)),
														A2(
														elm$html$Html$tr,
														_List_Nil,
														A2(
															elm$core$List$indexedMap,
															F2(
																function (numIndex, _n8) {
																	var mNum = _n8.a;
																	var rawC = _n8.b;
																	var c = elm$core$Char$toUpper(rawC);
																	var validCls = function () {
																		if (mNum.$ === 1) {
																			return 'unentered';
																		} else {
																			var num = mNum.a;
																			return A2(
																				elm$core$Maybe$withDefault,
																				false,
																				A2(
																					elm$core$Maybe$map,
																					function (qC) {
																						return _Utils_eq(
																							c,
																							elm$core$Char$toUpper(qC));
																					},
																					A2(author$project$Puzzle$quoteIndex, puzzle, num))) ? 'valid' : 'invalid';
																		}
																	}();
																	return A2(
																		elm$html$Html$td,
																		_List_fromArray(
																			[
																				elm$html$Html$Attributes$class('clue-numbering-number'),
																				elm$html$Html$Attributes$class(validCls)
																			]),
																		_List_fromArray(
																			[
																				A3(numberingFor, numIndex, mNum, c)
																			]));
																}),
															answer))
													])),
												function () {
												var editableWarning = (puzzle.H !== 2) ? elm$core$Maybe$Just(
													A2(
														elm$html$Html$span,
														_List_Nil,
														_List_fromArray(
															[
																elm$html$Html$text('Editing the clue will erase any numbers you have entered.')
															]))) : elm$core$Maybe$Nothing;
												var duplicateWarning = (!elm$core$List$isEmpty(dupWords)) ? elm$core$Maybe$Just(
													A2(
														elm$html$Html$span,
														_List_Nil,
														_List_fromArray(
															[
																elm$html$Html$text('Highlighted clue letters come from the same word.')
															]))) : elm$core$Maybe$Nothing;
												var descendingWarning = (fullyNumbered && _Utils_eq(
													elm$core$List$sort(unindexedClueNumbers),
													elm$core$List$reverse(unindexedClueNumbers))) ? elm$core$Maybe$Just(
													A2(
														elm$html$Html$span,
														_List_Nil,
														_List_fromArray(
															[
																elm$html$Html$text('Clue numbers are a descending run.')
															]))) : elm$core$Maybe$Nothing;
												var ascendingWarning = (fullyNumbered && _Utils_eq(
													elm$core$List$sort(unindexedClueNumbers),
													unindexedClueNumbers)) ? elm$core$Maybe$Just(
													A2(
														elm$html$Html$span,
														_List_Nil,
														_List_fromArray(
															[
																elm$html$Html$text('Clue numbers are an ascending run.')
															]))) : elm$core$Maybe$Nothing;
												var warnings = _List_fromArray(
													[editableWarning, ascendingWarning, descendingWarning, duplicateWarning]);
												return A2(
													elm$html$Html$div,
													_List_fromArray(
														[
															elm$html$Html$Attributes$class('warnings')
														]),
													A2(elm$core$List$filterMap, elm$core$Basics$identity, warnings));
											}()
											]));
								},
								model.t);
					}
				}()),
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('messages')
					]),
				_List_fromArray(
					[
						A2(
						elm$html$Html$div,
						_List_fromArray(
							[
								elm$html$Html$Attributes$id('progress')
							]),
						_Utils_ap(
							(!elm$core$Dict$isEmpty(model.A)) ? _List_fromArray(
								[
									A2(
									elm$html$Html$h3,
									_List_Nil,
									_List_fromArray(
										[
											elm$html$Html$text('Loading')
										]))
								]) : _List_Nil,
							A2(
								elm$core$List$map,
								function (_n17) {
									var s = _n17.a;
									var recvd = _n17.b;
									var tag = 'progress-' + s;
									return A2(
										elm$html$Html$div,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												elm$html$Html$meter,
												_List_fromArray(
													[
														elm$html$Html$Attributes$min('0.0'),
														elm$html$Html$Attributes$max('1.0'),
														elm$html$Html$Attributes$value(
														elm$core$String$fromFloat(recvd)),
														elm$html$Html$Attributes$id(tag)
													]),
												_List_Nil),
												A2(
												elm$html$Html$label,
												_List_fromArray(
													[
														elm$html$Html$Attributes$for(tag)
													]),
												_List_fromArray(
													[
														elm$html$Html$text(s)
													]))
											]));
								},
								elm$core$Dict$toList(model.A))))
					])),
				A2(
				elm$html$Html$section,
				_List_fromArray(
					[
						elm$html$Html$Attributes$id('debug'),
						A2(elm$html$Html$Attributes$style, 'display', 'none')
					]),
				_List_Nil)
			]));
};
var elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var elm$browser$Browser$Dom$NotFound = elm$core$Basics$identity;
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$url$Url$Http = 0;
var elm$url$Url$Https = 1;
var elm$core$String$indexes = _String_indexes;
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {aU: fragment, aZ: host, a7: path, a9: port_, bd: protocol, be: query};
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
					if (_n1.$ === 1) {
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
		0,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		1,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$element = _Browser_element;
var author$project$Main$main = elm$browser$Browser$element(
	{bU: author$project$Main$init, cg: author$project$Main$subscriptions, cn: author$project$Main$update, co: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(elm$json$Json$Decode$value)(0)}});}(this));