(() => { // webpackBootstrap
"use strict";
var __webpack_modules__ = ({
"./node_modules/.pnpm/uplot@1.6.32/node_modules/uplot/dist/uPlot.esm.js": 
/*!******************************************************************************!*\
  !*** ./node_modules/.pnpm/uplot@1.6.32/node_modules/uplot/dist/uPlot.esm.js ***!
  \******************************************************************************/
(function (__unused_webpack_module, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  "default": () => (uPlot)
});
/**
* Copyright (c) 2025, Leon Sorokin
* All rights reserved. (MIT Licensed)
*
* uPlot.js (μPlot)
* A small, fast chart for time series, lines, areas, ohlc & bars
* https://github.com/leeoniya/uPlot (v1.6.32)
*/

const FEAT_TIME          = true;

const pre = "u-";

const UPLOT          =       "uplot";
const ORI_HZ         = pre + "hz";
const ORI_VT         = pre + "vt";
const TITLE          = pre + "title";
const WRAP           = pre + "wrap";
const UNDER          = pre + "under";
const OVER           = pre + "over";
const AXIS           = pre + "axis";
const OFF            = pre + "off";
const SELECT         = pre + "select";
const CURSOR_X       = pre + "cursor-x";
const CURSOR_Y       = pre + "cursor-y";
const CURSOR_PT      = pre + "cursor-pt";
const LEGEND         = pre + "legend";
const LEGEND_LIVE    = pre + "live";
const LEGEND_INLINE  = pre + "inline";
const LEGEND_SERIES  = pre + "series";
const LEGEND_MARKER  = pre + "marker";
const LEGEND_LABEL   = pre + "label";
const LEGEND_VALUE   = pre + "value";

const WIDTH       = "width";
const HEIGHT      = "height";
const TOP         = "top";
const BOTTOM      = "bottom";
const LEFT        = "left";
const RIGHT       = "right";
const hexBlack    = "#000";
const transparent = hexBlack + "0";

const mousemove   = "mousemove";
const mousedown   = "mousedown";
const mouseup     = "mouseup";
const mouseenter  = "mouseenter";
const mouseleave  = "mouseleave";
const dblclick    = "dblclick";
const resize      = "resize";
const scroll      = "scroll";

const change      = "change";
const dppxchange  = "dppxchange";

const LEGEND_DISP = "--";

const domEnv = typeof window != 'undefined';

const doc = domEnv ? document  : null;
const win = domEnv ? window    : null;
const nav = domEnv ? navigator : null;

let pxRatio;

//export const canHover = domEnv && !win.matchMedia('(hover: none)').matches;

let query;

function setPxRatio() {
	let _pxRatio = devicePixelRatio;

	// during print preview, Chrome fires off these dppx queries even without changes
	if (pxRatio != _pxRatio) {
		pxRatio = _pxRatio;

		query && off(change, query, setPxRatio);
		query = matchMedia(`(min-resolution: ${pxRatio - 0.001}dppx) and (max-resolution: ${pxRatio + 0.001}dppx)`);
		on(change, query, setPxRatio);

		win.dispatchEvent(new CustomEvent(dppxchange));
	}
}

function addClass(el, c) {
	if (c != null) {
		let cl = el.classList;
		!cl.contains(c) && cl.add(c);
	}
}

function remClass(el, c) {
	let cl = el.classList;
	cl.contains(c) && cl.remove(c);
}

function setStylePx(el, name, value) {
	el.style[name] = value + "px";
}

function placeTag(tag, cls, targ, refEl) {
	let el = doc.createElement(tag);

	if (cls != null)
		addClass(el, cls);

	if (targ != null)
		targ.insertBefore(el, refEl);

	return el;
}

function placeDiv(cls, targ) {
	return placeTag("div", cls, targ);
}

const xformCache = new WeakMap();

function elTrans(el, xPos, yPos, xMax, yMax) {
	let xform = "translate(" + xPos + "px," + yPos + "px)";
	let xformOld = xformCache.get(el);

	if (xform != xformOld) {
		el.style.transform = xform;
		xformCache.set(el, xform);

		if (xPos < 0 || yPos < 0 || xPos > xMax || yPos > yMax)
			addClass(el, OFF);
		else
			remClass(el, OFF);
	}
}

const colorCache = new WeakMap();

function elColor(el, background, borderColor) {
	let newColor = background + borderColor;
	let oldColor = colorCache.get(el);

	if (newColor != oldColor) {
		colorCache.set(el, newColor);
		el.style.background = background;
		el.style.borderColor = borderColor;
	}
}

const sizeCache = new WeakMap();

function elSize(el, newWid, newHgt, centered) {
	let newSize = newWid + "" + newHgt;
	let oldSize = sizeCache.get(el);

	if (newSize != oldSize) {
		sizeCache.set(el, newSize);
		el.style.height = newHgt + "px";
		el.style.width = newWid + "px";
		el.style.marginLeft = centered ? -newWid/2 + "px" : 0;
		el.style.marginTop = centered ? -newHgt/2 + "px" : 0;
	}
}

const evOpts = {passive: true};
const evOpts2 = {...evOpts, capture: true};

function on(ev, el, cb, capt) {
	el.addEventListener(ev, cb, capt ? evOpts2 : evOpts);
}

function off(ev, el, cb, capt) {
	el.removeEventListener(ev, cb, evOpts);
}

domEnv && setPxRatio();

// binary search for index of closest value
function closestIdx(num, arr, lo, hi) {
	let mid;
	lo = lo || 0;
	hi = hi || arr.length - 1;
	let bitwise = hi <= 2147483647;

	while (hi - lo > 1) {
		mid = bitwise ? (lo + hi) >> 1 : floor((lo + hi) / 2);

		if (arr[mid] < num)
			lo = mid;
		else
			hi = mid;
	}

	if (num - arr[lo] <= arr[hi] - num)
		return lo;

	return hi;
}

function makeIndexOfs(predicate) {
	 let indexOfs = (data, _i0, _i1) => {
		let i0 = -1;
		let i1 = -1;

		for (let i = _i0; i <= _i1; i++) {
			if (predicate(data[i])) {
				i0 = i;
				break;
			}
		}

		for (let i = _i1; i >= _i0; i--) {
			if (predicate(data[i])) {
				i1 = i;
				break;
			}
		}

		return [i0, i1];
	 };

	 return indexOfs;
}

const notNullish = v => v != null;
const isPositive = v => v != null && v > 0;

const nonNullIdxs = makeIndexOfs(notNullish);
const positiveIdxs = makeIndexOfs(isPositive);

function getMinMax(data, _i0, _i1, sorted = 0, log = false) {
//	console.log("getMinMax()");

	let getEdgeIdxs = log ? positiveIdxs : nonNullIdxs;
	let predicate = log ? isPositive : notNullish;

	[_i0, _i1] = getEdgeIdxs(data, _i0, _i1);

	let _min = data[_i0];
	let _max = data[_i0];

	if (_i0 > -1) {
		if (sorted == 1) {
			_min = data[_i0];
			_max = data[_i1];
		}
		else if (sorted == -1) {
			_min = data[_i1];
			_max = data[_i0];
		}
		else {
			for (let i = _i0; i <= _i1; i++) {
				let v = data[i];

				if (predicate(v)) {
					if (v < _min)
						_min = v;
					else if (v > _max)
						_max = v;
				}
			}
		}
	}

	return [_min ?? inf, _max ?? -inf]; // todo: fix to return nulls
}

function rangeLog(min, max, base, fullMags) {
	let minSign = sign(min);
	let maxSign = sign(max);

	if (min == max) {
		if (minSign == -1) {
			min *= base;
			max /= base;
		}
		else {
			min /= base;
			max *= base;
		}
	}

	let logFn = base == 10 ? log10 : log2;

	let growMinAbs = minSign == 1 ? floor : ceil;
	let growMaxAbs = maxSign == 1 ? ceil : floor;

	let minExp = growMinAbs(logFn(abs(min)));
	let maxExp = growMaxAbs(logFn(abs(max)));

	let minIncr = pow(base, minExp);
	let maxIncr = pow(base, maxExp);

	// fix values like Math.pow(10, -5) === 0.000009999999999999999
	if (base == 10) {
		if (minExp < 0)
			minIncr = roundDec(minIncr, -minExp);
		if (maxExp < 0)
			maxIncr = roundDec(maxIncr, -maxExp);
	}

	if (fullMags || base == 2) {
		min = minIncr * minSign;
		max = maxIncr * maxSign;
	}
	else {
		min = incrRoundDn(min, minIncr);
		max = incrRoundUp(max, maxIncr);
	}

	return [min, max];
}

function rangeAsinh(min, max, base, fullMags) {
	let minMax = rangeLog(min, max, base, fullMags);

	if (min == 0)
		minMax[0] = 0;

	if (max == 0)
		minMax[1] = 0;

	return minMax;
}

const rangePad = 0.1;

const autoRangePart = {
	mode: 3,
	pad: rangePad,
};

const _eqRangePart = {
	pad:  0,
	soft: null,
	mode: 0,
};

const _eqRange = {
	min: _eqRangePart,
	max: _eqRangePart,
};

// this ensures that non-temporal/numeric y-axes get multiple-snapped padding added above/below
// TODO: also account for incrs when snapping to ensure top of axis gets a tick & value
function rangeNum(_min, _max, mult, extra) {
	if (isObj(mult))
		return _rangeNum(_min, _max, mult);

	_eqRangePart.pad  = mult;
	_eqRangePart.soft = extra ? 0 : null;
	_eqRangePart.mode = extra ? 3 : 0;

	return _rangeNum(_min, _max, _eqRange);
}

// nullish coalesce
function ifNull(lh, rh) {
	return lh == null ? rh : lh;
}

// checks if given index range in an array contains a non-null value
// aka a range-bounded Array.some()
function hasData(data, idx0, idx1) {
	idx0 = ifNull(idx0, 0);
	idx1 = ifNull(idx1, data.length - 1);

	while (idx0 <= idx1) {
		if (data[idx0] != null)
			return true;
		idx0++;
	}

	return false;
}

function _rangeNum(_min, _max, cfg) {
	let cmin = cfg.min;
	let cmax = cfg.max;

	let padMin = ifNull(cmin.pad, 0);
	let padMax = ifNull(cmax.pad, 0);

	let hardMin = ifNull(cmin.hard, -inf);
	let hardMax = ifNull(cmax.hard,  inf);

	let softMin = ifNull(cmin.soft,  inf);
	let softMax = ifNull(cmax.soft, -inf);

	let softMinMode = ifNull(cmin.mode, 0);
	let softMaxMode = ifNull(cmax.mode, 0);

	let delta = _max - _min;
	let deltaMag = log10(delta);

	let scalarMax = max(abs(_min), abs(_max));
	let scalarMag = log10(scalarMax);

	let scalarMagDelta = abs(scalarMag - deltaMag);

	// this handles situations like 89.7, 89.69999999999999
	// by assuming 0.001x deltas are precision errors
//	if (delta > 0 && delta < abs(_max) / 1e3)
//		delta = 0;

	// treat data as flat if delta is less than 1e-24
	// or range is 11+ orders of magnitude below raw values, e.g. 99999999.99999996 - 100000000.00000004
	if (delta < 1e-24 || scalarMagDelta > 10) {
		delta = 0;

		// if soft mode is 2 and all vals are flat at 0, avoid the 0.1 * 1e3 fallback
		// this prevents 0,0,0 from ranging to -100,100 when softMin/softMax are -1,1
		if (_min == 0 || _max == 0) {
			delta = 1e-24;

			if (softMinMode == 2 && softMin != inf)
				padMin = 0;

			if (softMaxMode == 2 && softMax != -inf)
				padMax = 0;
		}
	}

	let nonZeroDelta = delta || scalarMax || 1e3;
	let mag          = log10(nonZeroDelta);
	let base         = pow(10, floor(mag));

	let _padMin  = nonZeroDelta * (delta == 0 ? (_min == 0 ? .1 : 1) : padMin);
	let _newMin  = roundDec(incrRoundDn(_min - _padMin, base/10), 24);
	let _softMin = _min >= softMin && (softMinMode == 1 || softMinMode == 3 && _newMin <= softMin || softMinMode == 2 && _newMin >= softMin) ? softMin : inf;
	let minLim   = max(hardMin, _newMin < _softMin && _min >= _softMin ? _softMin : min(_softMin, _newMin));

	let _padMax  = nonZeroDelta * (delta == 0 ? (_max == 0 ? .1 : 1) : padMax);
	let _newMax  = roundDec(incrRoundUp(_max + _padMax, base/10), 24);
	let _softMax = _max <= softMax && (softMaxMode == 1 || softMaxMode == 3 && _newMax >= softMax || softMaxMode == 2 && _newMax <= softMax) ? softMax : -inf;
	let maxLim   = min(hardMax, _newMax > _softMax && _max <= _softMax ? _softMax : max(_softMax, _newMax));

	if (minLim == maxLim && minLim == 0)
		maxLim = 100;

	return [minLim, maxLim];
}

// alternative: https://stackoverflow.com/a/2254896
const numFormatter = new Intl.NumberFormat(domEnv ? nav.language : 'en-US');
const fmtNum = val => numFormatter.format(val);

const M = Math;

const PI = M.PI;
const abs = M.abs;
const floor = M.floor;
const round = M.round;
const ceil = M.ceil;
const min = M.min;
const max = M.max;
const pow = M.pow;
const sign = M.sign;
const log10 = M.log10;
const log2 = M.log2;
// TODO: seems like this needs to match asinh impl if the passed v is tweaked?
const sinh =  (v, linthresh = 1) => M.sinh(v) * linthresh;
const asinh = (v, linthresh = 1) => M.asinh(v / linthresh);

const inf = Infinity;

function numIntDigits(x) {
	return (log10((x ^ (x >> 31)) - (x >> 31)) | 0) + 1;
}

function clamp(num, _min, _max) {
	return min(max(num, _min), _max);
}

function isFn(v) {
	return typeof v == "function";
}

function fnOrSelf(v) {
	return isFn(v) ? v : () => v;
}

const noop = () => {};

// note: these identity fns may get deoptimized if reused for different arg types
// a TS version would enforce they stay monotyped and require making variants
const retArg0 = _0 => _0;

const retArg1 = (_0, _1) => _1;

const retNull = _ => null;

const retTrue = _ => true;

const retEq = (a, b) => a == b;

const regex6 = /\.\d*?(?=9{6,}|0{6,})/gm;

// e.g. 17999.204999999998 -> 17999.205
const fixFloat = val => {
	if (isInt(val) || fixedDec.has(val))
		return val;

	const str = `${val}`;

	const match = str.match(regex6);

	if (match == null)
		return val;

	let len = match[0].length - 1;

	// e.g. 1.0000000000000001e-24
	if (str.indexOf('e-') != -1) {
		let [num, exp] = str.split('e');
		return +`${fixFloat(num)}e${exp}`;
	}

	return roundDec(val, len);
};

function incrRound(num, incr) {
	return fixFloat(roundDec(fixFloat(num/incr))*incr);
}

function incrRoundUp(num, incr) {
	return fixFloat(ceil(fixFloat(num/incr))*incr);
}

function incrRoundDn(num, incr) {
	return fixFloat(floor(fixFloat(num/incr))*incr);
}

// https://stackoverflow.com/a/48764436
// rounds half away from zero
function roundDec(val, dec = 0) {
	if (isInt(val))
		return val;
//	else if (dec == 0)
//		return round(val);

	let p = 10 ** dec;
	let n = (val * p) * (1 + Number.EPSILON);
	return round(n) / p;
}

const fixedDec = new Map();

function guessDec(num) {
	return ((""+num).split(".")[1] || "").length;
}

function genIncrs(base, minExp, maxExp, mults) {
	let incrs = [];

	let multDec = mults.map(guessDec);

	for (let exp = minExp; exp < maxExp; exp++) {
		let expa = abs(exp);
		let mag = roundDec(pow(base, exp), expa);

		for (let i = 0; i < mults.length; i++) {
			let _incr = base == 10 ? +`${mults[i]}e${exp}` : mults[i] * mag;
			let dec = (exp >= 0 ? 0 : expa) + (exp >= multDec[i] ? 0 : multDec[i]);
			let incr = base == 10 ? _incr : roundDec(_incr, dec);
			incrs.push(incr);
			fixedDec.set(incr, dec);
		}
	}

	return incrs;
}

//export const assign = Object.assign;

const EMPTY_OBJ = {};
const EMPTY_ARR = [];

const nullNullTuple = [null, null];

const isArr = Array.isArray;
const isInt = Number.isInteger;
const isUndef = v => v === void 0;

function isStr(v) {
	return typeof v == 'string';
}

function isObj(v) {
	let is = false;

	if (v != null) {
		let c = v.constructor;
		is = c == null || c == Object;
	}

	return is;
}

function fastIsObj(v) {
	return v != null && typeof v == 'object';
}

const TypedArray = Object.getPrototypeOf(Uint8Array);

const __proto__ = "__proto__";

function copy(o, _isObj = isObj) {
	let out;

	if (isArr(o)) {
		let val = o.find(v => v != null);

		if (isArr(val) || _isObj(val)) {
			out = Array(o.length);
			for (let i = 0; i < o.length; i++)
				out[i] = copy(o[i], _isObj);
		}
		else
			out = o.slice();
	}
	else if (o instanceof TypedArray) // also (ArrayBuffer.isView(o) && !(o instanceof DataView))
		out = o.slice();
	else if (_isObj(o)) {
		out = {};
		for (let k in o) {
			if (k != __proto__)
				out[k] = copy(o[k], _isObj);
		}
	}
	else
		out = o;

	return out;
}

function assign(targ) {
	let args = arguments;

	for (let i = 1; i < args.length; i++) {
		let src = args[i];

		for (let key in src) {
			if (key != __proto__) {
				if (isObj(targ[key]))
					assign(targ[key], copy(src[key]));
				else
					targ[key] = copy(src[key]);
			}
		}
	}

	return targ;
}

// nullModes
const NULL_REMOVE = 0;  // nulls are converted to undefined (e.g. for spanGaps: true)
const NULL_RETAIN = 1;  // nulls are retained, with alignment artifacts set to undefined (default)
const NULL_EXPAND = 2;  // nulls are expanded to include any adjacent alignment artifacts

// sets undefined values to nulls when adjacent to existing nulls (minesweeper)
function nullExpand(yVals, nullIdxs, alignedLen) {
	for (let i = 0, xi, lastNullIdx = -1; i < nullIdxs.length; i++) {
		let nullIdx = nullIdxs[i];

		if (nullIdx > lastNullIdx) {
			xi = nullIdx - 1;
			while (xi >= 0 && yVals[xi] == null)
				yVals[xi--] = null;

			xi = nullIdx + 1;
			while (xi < alignedLen && yVals[xi] == null)
				yVals[lastNullIdx = xi++] = null;
		}
	}
}

// nullModes is a tables-matched array indicating how to treat nulls in each series
// output is sorted ASC on the joined field (table[0]) and duplicate join values are collapsed
function join(tables, nullModes) {
	if (allHeadersSame(tables)) {
	//	console.log('cheap join!');

		let table = tables[0].slice();

		for (let i = 1; i < tables.length; i++)
			table.push(...tables[i].slice(1));

		if (!isAsc(table[0]))
			table = sortCols(table);

		return table;
	}

	let xVals = new Set();

	for (let ti = 0; ti < tables.length; ti++) {
		let t = tables[ti];
		let xs = t[0];
		let len = xs.length;

		for (let i = 0; i < len; i++)
			xVals.add(xs[i]);
	}

	let data = [Array.from(xVals).sort((a, b) => a - b)];

	let alignedLen = data[0].length;

	let xIdxs = new Map();

	for (let i = 0; i < alignedLen; i++)
		xIdxs.set(data[0][i], i);

	for (let ti = 0; ti < tables.length; ti++) {
		let t = tables[ti];
		let xs = t[0];

		for (let si = 1; si < t.length; si++) {
			let ys = t[si];

			let yVals = Array(alignedLen).fill(undefined);

			let nullMode = nullModes ? nullModes[ti][si] : NULL_RETAIN;

			let nullIdxs = [];

			for (let i = 0; i < ys.length; i++) {
				let yVal = ys[i];
				let alignedIdx = xIdxs.get(xs[i]);

				if (yVal === null) {
					if (nullMode != NULL_REMOVE) {
						yVals[alignedIdx] = yVal;

						if (nullMode == NULL_EXPAND)
							nullIdxs.push(alignedIdx);
					}
				}
				else
					yVals[alignedIdx] = yVal;
			}

			nullExpand(yVals, nullIdxs, alignedLen);

			data.push(yVals);
		}
	}

	return data;
}

const microTask = typeof queueMicrotask == "undefined" ? fn => Promise.resolve().then(fn) : queueMicrotask;

// TODO: https://github.com/dy/sort-ids (~2x faster for 1e5+ arrays)
function sortCols(table) {
	let head = table[0];
	let rlen = head.length;

	let idxs = Array(rlen);
	for (let i = 0; i < idxs.length; i++)
		idxs[i] = i;

	idxs.sort((i0, i1) => head[i0] - head[i1]);

	let table2 = [];
	for (let i = 0; i < table.length; i++) {
		let row = table[i];
		let row2 = Array(rlen);

		for (let j = 0; j < rlen; j++)
			row2[j] = row[idxs[j]];

		table2.push(row2);
	}

	return table2;
}

// test if we can do cheap join (all join fields same)
function allHeadersSame(tables) {
	let vals0 = tables[0][0];
	let len0 = vals0.length;

	for (let i = 1; i < tables.length; i++) {
		let vals1 = tables[i][0];

		if (vals1.length != len0)
			return false;

		if (vals1 != vals0) {
			for (let j = 0; j < len0; j++) {
				if (vals1[j] != vals0[j])
					return false;
			}
		}
	}

	return true;
}

function isAsc(vals, samples = 100) {
	const len = vals.length;

	// empty or single value
	if (len <= 1)
		return true;

	// skip leading & trailing nullish
	let firstIdx = 0;
	let lastIdx = len - 1;

	while (firstIdx <= lastIdx && vals[firstIdx] == null)
		firstIdx++;

	while (lastIdx >= firstIdx && vals[lastIdx] == null)
		lastIdx--;

	// all nullish or one value surrounded by nullish
	if (lastIdx <= firstIdx)
		return true;

	const stride = max(1, floor((lastIdx - firstIdx + 1) / samples));

	for (let prevVal = vals[firstIdx], i = firstIdx + stride; i <= lastIdx; i += stride) {
		const v = vals[i];

		if (v != null) {
			if (v <= prevVal)
				return false;

			prevVal = v;
		}
	}

	return true;
}

const months = [
	"January",
	"February",
	"March",
	"April",
	"May",
	"June",
	"July",
	"August",
	"September",
	"October",
	"November",
	"December",
];

const days = [
	"Sunday",
	"Monday",
	"Tuesday",
	"Wednesday",
	"Thursday",
	"Friday",
	"Saturday",
];

function slice3(str) {
	return str.slice(0, 3);
}

const days3 = days.map(slice3);

const months3 = months.map(slice3);

const engNames = {
	MMMM: months,
	MMM:  months3,
	WWWW: days,
	WWW:  days3,
};

function zeroPad2(int) {
	return (int < 10 ? '0' : '') + int;
}

function zeroPad3(int) {
	return (int < 10 ? '00' : int < 100 ? '0' : '') + int;
}

/*
function suffix(int) {
	let mod10 = int % 10;

	return int + (
		mod10 == 1 && int != 11 ? "st" :
		mod10 == 2 && int != 12 ? "nd" :
		mod10 == 3 && int != 13 ? "rd" : "th"
	);
}
*/

const subs = {
	// 2019
	YYYY:	d => d.getFullYear(),
	// 19
	YY:		d => (d.getFullYear()+'').slice(2),
	// July
	MMMM:	(d, names) => names.MMMM[d.getMonth()],
	// Jul
	MMM:	(d, names) => names.MMM[d.getMonth()],
	// 07
	MM:		d => zeroPad2(d.getMonth()+1),
	// 7
	M:		d => d.getMonth()+1,
	// 09
	DD:		d => zeroPad2(d.getDate()),
	// 9
	D:		d => d.getDate(),
	// Monday
	WWWW:	(d, names) => names.WWWW[d.getDay()],
	// Mon
	WWW:	(d, names) => names.WWW[d.getDay()],
	// 03
	HH:		d => zeroPad2(d.getHours()),
	// 3
	H:		d => d.getHours(),
	// 9 (12hr, unpadded)
	h:		d => {let h = d.getHours(); return h == 0 ? 12 : h > 12 ? h - 12 : h;},
	// AM
	AA:		d => d.getHours() >= 12 ? 'PM' : 'AM',
	// am
	aa:		d => d.getHours() >= 12 ? 'pm' : 'am',
	// a
	a:		d => d.getHours() >= 12 ? 'p' : 'a',
	// 09
	mm:		d => zeroPad2(d.getMinutes()),
	// 9
	m:		d => d.getMinutes(),
	// 09
	ss:		d => zeroPad2(d.getSeconds()),
	// 9
	s:		d => d.getSeconds(),
	// 374
	fff:	d => zeroPad3(d.getMilliseconds()),
};

function fmtDate(tpl, names) {
	names = names || engNames;
	let parts = [];

	let R = /\{([a-z]+)\}|[^{]+/gi, m;

	while (m = R.exec(tpl))
		parts.push(m[0][0] == '{' ? subs[m[1]] : m[0]);

	return d => {
		let out = '';

		for (let i = 0; i < parts.length; i++)
			out += typeof parts[i] == "string" ? parts[i] : parts[i](d, names);

		return out;
	}
}

const localTz = new Intl.DateTimeFormat().resolvedOptions().timeZone;

// https://stackoverflow.com/questions/15141762/how-to-initialize-a-javascript-date-to-a-particular-time-zone/53652131#53652131
function tzDate(date, tz) {
	let date2;

	// perf optimization
	if (tz == 'UTC' || tz == 'Etc/UTC')
		date2 = new Date(+date + date.getTimezoneOffset() * 6e4);
	else if (tz == localTz)
		date2 = date;
	else {
		date2 = new Date(date.toLocaleString('en-US', {timeZone: tz}));
		date2.setMilliseconds(date.getMilliseconds());
	}

	return date2;
}

//export const series = [];

// default formatters:

const onlyWhole = v => v % 1 == 0;

const allMults = [1,2,2.5,5];

// ...0.01, 0.02, 0.025, 0.05, 0.1, 0.2, 0.25, 0.5
const decIncrs = genIncrs(10, -32, 0, allMults);

// 1, 2, 2.5, 5, 10, 20, 25, 50...
const oneIncrs = genIncrs(10, 0, 32, allMults);

// 1, 2,      5, 10, 20, 25, 50...
const wholeIncrs = oneIncrs.filter(onlyWhole);

const numIncrs = decIncrs.concat(oneIncrs);

const NL = "\n";

const yyyy    = "{YYYY}";
const NLyyyy  = NL + yyyy;
const md      = "{M}/{D}";
const NLmd    = NL + md;
const NLmdyy  = NLmd + "/{YY}";

const aa      = "{aa}";
const hmm     = "{h}:{mm}";
const hmmaa   = hmm + aa;
const NLhmmaa = NL + hmmaa;
const ss      = ":{ss}";

const _ = null;

function genTimeStuffs(ms) {
	let	s  = ms * 1e3,
		m  = s  * 60,
		h  = m  * 60,
		d  = h  * 24,
		mo = d  * 30,
		y  = d  * 365;

	// min of 1e-3 prevents setting a temporal x ticks too small since Date objects cannot advance ticks smaller than 1ms
	let subSecIncrs = ms == 1 ? genIncrs(10, 0, 3, allMults).filter(onlyWhole) : genIncrs(10, -3, 0, allMults);

	let timeIncrs = subSecIncrs.concat([
		// minute divisors (# of secs)
		s,
		s * 5,
		s * 10,
		s * 15,
		s * 30,
		// hour divisors (# of mins)
		m,
		m * 5,
		m * 10,
		m * 15,
		m * 30,
		// day divisors (# of hrs)
		h,
		h * 2,
		h * 3,
		h * 4,
		h * 6,
		h * 8,
		h * 12,
		// month divisors TODO: need more?
		d,
		d * 2,
		d * 3,
		d * 4,
		d * 5,
		d * 6,
		d * 7,
		d * 8,
		d * 9,
		d * 10,
		d * 15,
		// year divisors (# months, approx)
		mo,
		mo * 2,
		mo * 3,
		mo * 4,
		mo * 6,
		// century divisors
		y,
		y * 2,
		y * 5,
		y * 10,
		y * 25,
		y * 50,
		y * 100,
	]);

	// [0]:   minimum num secs in the tick incr
	// [1]:   default tick format
	// [2-7]: rollover tick formats
	// [8]:   mode: 0: replace [1] -> [2-7], 1: concat [1] + [2-7]
	const _timeAxisStamps = [
	//   tick incr    default          year                    month   day                   hour    min       sec   mode
		[y,           yyyy,            _,                      _,      _,                    _,      _,        _,       1],
		[d * 28,      "{MMM}",         NLyyyy,                 _,      _,                    _,      _,        _,       1],
		[d,           md,              NLyyyy,                 _,      _,                    _,      _,        _,       1],
		[h,           "{h}" + aa,      NLmdyy,                 _,      NLmd,                 _,      _,        _,       1],
		[m,           hmmaa,           NLmdyy,                 _,      NLmd,                 _,      _,        _,       1],
		[s,           ss,              NLmdyy + " " + hmmaa,   _,      NLmd + " " + hmmaa,   _,      NLhmmaa,  _,       1],
		[ms,          ss + ".{fff}",   NLmdyy + " " + hmmaa,   _,      NLmd + " " + hmmaa,   _,      NLhmmaa,  _,       1],
	];

	// the ensures that axis ticks, values & grid are aligned to logical temporal breakpoints and not an arbitrary timestamp
	// https://www.timeanddate.com/time/dst/
	// https://www.timeanddate.com/time/dst/2019.html
	// https://www.epochconverter.com/timezones
	function timeAxisSplits(tzDate) {
		return (self, axisIdx, scaleMin, scaleMax, foundIncr, foundSpace) => {
			let splits = [];
			let isYr = foundIncr >= y;
			let isMo = foundIncr >= mo && foundIncr < y;

			// get the timezone-adjusted date
			let minDate = tzDate(scaleMin);
			let minDateTs = roundDec(minDate * ms, 3);

			// get ts of 12am (this lands us at or before the original scaleMin)
			let minMin = mkDate(minDate.getFullYear(), isYr ? 0 : minDate.getMonth(), isMo || isYr ? 1 : minDate.getDate());
			let minMinTs = roundDec(minMin * ms, 3);

			if (isMo || isYr) {
				let moIncr = isMo ? foundIncr / mo : 0;
				let yrIncr = isYr ? foundIncr / y  : 0;
			//	let tzOffset = scaleMin - minDateTs;		// needed?
				let split = minDateTs == minMinTs ? minDateTs : roundDec(mkDate(minMin.getFullYear() + yrIncr, minMin.getMonth() + moIncr, 1) * ms, 3);
				let splitDate = new Date(round(split / ms));
				let baseYear = splitDate.getFullYear();
				let baseMonth = splitDate.getMonth();

				for (let i = 0; split <= scaleMax; i++) {
					let next = mkDate(baseYear + yrIncr * i, baseMonth + moIncr * i, 1);
					let offs = next - tzDate(roundDec(next * ms, 3));

					split = roundDec((+next + offs) * ms, 3);

					if (split <= scaleMax)
						splits.push(split);
				}
			}
			else {
				let incr0 = foundIncr >= d ? d : foundIncr;
				let tzOffset = floor(scaleMin) - floor(minDateTs);
				let split = minMinTs + tzOffset + incrRoundUp(minDateTs - minMinTs, incr0);
				splits.push(split);

				let date0 = tzDate(split);

				let prevHour = date0.getHours() + (date0.getMinutes() / m) + (date0.getSeconds() / h);
				let incrHours = foundIncr / h;

				let minSpace = self.axes[axisIdx]._space;
				let pctSpace = foundSpace / minSpace;

				while (1) {
					split = roundDec(split + foundIncr, ms == 1 ? 0 : 3);

					if (split > scaleMax)
						break;

					if (incrHours > 1) {
						let expectedHour = floor(roundDec(prevHour + incrHours, 6)) % 24;
						let splitDate = tzDate(split);
						let actualHour = splitDate.getHours();

						let dstShift = actualHour - expectedHour;

						if (dstShift > 1)
							dstShift = -1;

						split -= dstShift * h;

						prevHour = (prevHour + incrHours) % 24;

						// add a tick only if it's further than 70% of the min allowed label spacing
						let prevSplit = splits[splits.length - 1];
						let pctIncr = roundDec((split - prevSplit) / foundIncr, 3);

						if (pctIncr * pctSpace >= .7)
							splits.push(split);
					}
					else
						splits.push(split);
				}
			}

			return splits;
		}
	}

	return [
		timeIncrs,
		_timeAxisStamps,
		timeAxisSplits,
	];
}

const [ timeIncrsMs, _timeAxisStampsMs, timeAxisSplitsMs ] = genTimeStuffs(1);
const [ timeIncrsS,  _timeAxisStampsS,  timeAxisSplitsS  ] = genTimeStuffs(1e-3);

// base 2
genIncrs(2, -53, 53, [1]);

/*
console.log({
	decIncrs,
	oneIncrs,
	wholeIncrs,
	numIncrs,
	timeIncrs,
	fixedDec,
});
*/

function timeAxisStamps(stampCfg, fmtDate) {
	return stampCfg.map(s => s.map((v, i) =>
		i == 0 || i == 8 || v == null ? v : fmtDate(i == 1 || s[8] == 0 ? v : s[1] + v)
	));
}

// TODO: will need to accept spaces[] and pull incr into the loop when grid will be non-uniform, eg for log scales.
// currently we ignore this for months since they're *nearly* uniform and the added complexity is not worth it
function timeAxisVals(tzDate, stamps) {
	return (self, splits, axisIdx, foundSpace, foundIncr) => {
		let s = stamps.find(s => foundIncr >= s[0]) || stamps[stamps.length - 1];

		// these track boundaries when a full label is needed again
		let prevYear;
		let prevMnth;
		let prevDate;
		let prevHour;
		let prevMins;
		let prevSecs;

		return splits.map(split => {
			let date = tzDate(split);

			let newYear = date.getFullYear();
			let newMnth = date.getMonth();
			let newDate = date.getDate();
			let newHour = date.getHours();
			let newMins = date.getMinutes();
			let newSecs = date.getSeconds();

			let stamp = (
				newYear != prevYear && s[2] ||
				newMnth != prevMnth && s[3] ||
				newDate != prevDate && s[4] ||
				newHour != prevHour && s[5] ||
				newMins != prevMins && s[6] ||
				newSecs != prevSecs && s[7] ||
				                       s[1]
			);

			prevYear = newYear;
			prevMnth = newMnth;
			prevDate = newDate;
			prevHour = newHour;
			prevMins = newMins;
			prevSecs = newSecs;

			return stamp(date);
		});
	}
}

// for when axis.values is defined as a static fmtDate template string
function timeAxisVal(tzDate, dateTpl) {
	let stamp = fmtDate(dateTpl);
	return (self, splits, axisIdx, foundSpace, foundIncr) => splits.map(split => stamp(tzDate(split)));
}

function mkDate(y, m, d) {
	return new Date(y, m, d);
}

function timeSeriesStamp(stampCfg, fmtDate) {
	return fmtDate(stampCfg);
}
const _timeSeriesStamp = '{YYYY}-{MM}-{DD} {h}:{mm}{aa}';

function timeSeriesVal(tzDate, stamp) {
	return (self, val, seriesIdx, dataIdx) => dataIdx == null ? LEGEND_DISP : stamp(tzDate(val));
}

function legendStroke(self, seriesIdx) {
	let s = self.series[seriesIdx];
	return s.width ? s.stroke(self, seriesIdx) : s.points.width ? s.points.stroke(self, seriesIdx) : null;
}

function legendFill(self, seriesIdx) {
	return self.series[seriesIdx].fill(self, seriesIdx);
}

const legendOpts = {
	show: true,
	live: true,
	isolate: false,
	mount: noop,
	markers: {
		show: true,
		width: 2,
		stroke: legendStroke,
		fill: legendFill,
		dash: "solid",
	},
	idx: null,
	idxs: null,
	values: [],
};

function cursorPointShow(self, si) {
	let o = self.cursor.points;

	let pt = placeDiv();

	let size = o.size(self, si);
	setStylePx(pt, WIDTH, size);
	setStylePx(pt, HEIGHT, size);

	let mar = size / -2;
	setStylePx(pt, "marginLeft", mar);
	setStylePx(pt, "marginTop", mar);

	let width = o.width(self, si, size);
	width && setStylePx(pt, "borderWidth", width);

	return pt;
}

function cursorPointFill(self, si) {
	let sp = self.series[si].points;
	return sp._fill || sp._stroke;
}

function cursorPointStroke(self, si) {
	let sp = self.series[si].points;
	return sp._stroke || sp._fill;
}

function cursorPointSize(self, si) {
	let sp = self.series[si].points;
	return sp.size;
}

const moveTuple = [0,0];

function cursorMove(self, mouseLeft1, mouseTop1) {
	moveTuple[0] = mouseLeft1;
	moveTuple[1] = mouseTop1;
	return moveTuple;
}

function filtBtn0(self, targ, handle, onlyTarg = true) {
	return e => {
		e.button == 0 && (!onlyTarg || e.target == targ) && handle(e);
	};
}

function filtTarg(self, targ, handle, onlyTarg = true) {
	return e => {
		(!onlyTarg || e.target == targ) && handle(e);
	};
}

const cursorOpts = {
	show: true,
	x: true,
	y: true,
	lock: false,
	move: cursorMove,
	points: {
		one:    false,
		show:   cursorPointShow,
		size:   cursorPointSize,
		width:  0,
		stroke: cursorPointStroke,
		fill:   cursorPointFill,
	},

	bind: {
		mousedown:   filtBtn0,
		mouseup:     filtBtn0,
		click:       filtBtn0, // legend clicks, not .u-over clicks
		dblclick:    filtBtn0,

		mousemove:   filtTarg,
		mouseleave:  filtTarg,
		mouseenter:  filtTarg,
	},

	drag: {
		setScale: true,
		x: true,
		y: false,
		dist: 0,
		uni: null,
		click: (self, e) => {
		//	e.preventDefault();
			e.stopPropagation();
			e.stopImmediatePropagation();
		},
		_x: false,
		_y: false,
	},

	focus: {
		dist: (self, seriesIdx, dataIdx, valPos, curPos) => valPos - curPos,
		prox: -1,
		bias: 0,
	},

	hover: {
		skip: [void 0],
		prox: null,
		bias: 0,
	},

	left: -10,
	top: -10,
	idx: null,
	dataIdx: null,
	idxs: null,

	event: null,
};

const axisLines = {
	show: true,
	stroke: "rgba(0,0,0,0.07)",
	width: 2,
//	dash: [],
};

const grid = assign({}, axisLines, {
	filter: retArg1,
});

const ticks = assign({}, grid, {
	size: 10,
});

const border = assign({}, axisLines, {
	show: false,
});

const font      = '12px system-ui, -apple-system, "Segoe UI", Roboto, "Helvetica Neue", Arial, "Noto Sans", sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol", "Noto Color Emoji"';
const labelFont = "bold " + font;
const lineGap = 1.5;	// font-size multiplier

const xAxisOpts = {
	show: true,
	scale: "x",
	stroke: hexBlack,
	space: 50,
	gap: 5,
	alignTo: 1,
	size: 50,
	labelGap: 0,
	labelSize: 30,
	labelFont,
	side: 2,
//	class: "x-vals",
//	incrs: timeIncrs,
//	values: timeVals,
//	filter: retArg1,
	grid,
	ticks,
	border,
	font,
	lineGap,
	rotate: 0,
};

const numSeriesLabel = "Value";
const timeSeriesLabel = "Time";

const xSeriesOpts = {
	show: true,
	scale: "x",
	auto: false,
	sorted: 1,
//	label: "Time",
//	value: v => stamp(new Date(v * 1e3)),

	// internal caches
	min: inf,
	max: -inf,
	idxs: [],
};

function numAxisVals(self, splits, axisIdx, foundSpace, foundIncr) {
	return splits.map(v => v == null ? "" : fmtNum(v));
}

function numAxisSplits(self, axisIdx, scaleMin, scaleMax, foundIncr, foundSpace, forceMin) {
	let splits = [];

	let numDec = fixedDec.get(foundIncr) || 0;

	scaleMin = forceMin ? scaleMin : roundDec(incrRoundUp(scaleMin, foundIncr), numDec);

	for (let val = scaleMin; val <= scaleMax; val = roundDec(val + foundIncr, numDec))
		splits.push(Object.is(val, -0) ? 0 : val);		// coalesces -0

	return splits;
}

// this doesnt work for sin, which needs to come off from 0 independently in pos and neg dirs
function logAxisSplits(self, axisIdx, scaleMin, scaleMax, foundIncr, foundSpace, forceMin) {
	const splits = [];

	const logBase = self.scales[self.axes[axisIdx].scale].log;

	const logFn = logBase == 10 ? log10 : log2;

	const exp = floor(logFn(scaleMin));

	foundIncr = pow(logBase, exp);

	// boo: 10 ** -24 === 1.0000000000000001e-24
	// this grabs the proper 1e-24 one
	if (logBase == 10)
		foundIncr = numIncrs[closestIdx(foundIncr, numIncrs)];

	let split = scaleMin;
	let nextMagIncr = foundIncr * logBase;

	if (logBase == 10)
		nextMagIncr = numIncrs[closestIdx(nextMagIncr, numIncrs)];

	do {
		splits.push(split);
		split = split + foundIncr;

		if (logBase == 10 && !fixedDec.has(split))
			split = roundDec(split, fixedDec.get(foundIncr));

		if (split >= nextMagIncr) {
			foundIncr = split;
			nextMagIncr = foundIncr * logBase;

			if (logBase == 10)
				nextMagIncr = numIncrs[closestIdx(nextMagIncr, numIncrs)];
		}
	} while (split <= scaleMax);

	return splits;
}

function asinhAxisSplits(self, axisIdx, scaleMin, scaleMax, foundIncr, foundSpace, forceMin) {
	let sc = self.scales[self.axes[axisIdx].scale];

	let linthresh = sc.asinh;

	let posSplits = scaleMax > linthresh ? logAxisSplits(self, axisIdx, max(linthresh, scaleMin), scaleMax, foundIncr) : [linthresh];
	let zero = scaleMax >= 0 && scaleMin <= 0 ? [0] : [];
	let negSplits = scaleMin < -linthresh ? logAxisSplits(self, axisIdx, max(linthresh, -scaleMax), -scaleMin, foundIncr): [linthresh];

	return negSplits.reverse().map(v => -v).concat(zero, posSplits);
}

const RE_ALL   = /./;
const RE_12357 = /[12357]/;
const RE_125   = /[125]/;
const RE_1     = /1/;

const _filt = (splits, distr, re, keepMod) => splits.map((v, i) => ((distr == 4 && v == 0) || i % keepMod == 0 && re.test(v.toExponential()[v < 0 ? 1 : 0])) ? v : null);

function log10AxisValsFilt(self, splits, axisIdx, foundSpace, foundIncr) {
	let axis = self.axes[axisIdx];
	let scaleKey = axis.scale;
	let sc = self.scales[scaleKey];

//	if (sc.distr == 3 && sc.log == 2)
//		return splits;

	let valToPos = self.valToPos;

	let minSpace = axis._space;

	let _10 = valToPos(10, scaleKey);

	let re = (
		valToPos(9, scaleKey) - _10 >= minSpace ? RE_ALL :
		valToPos(7, scaleKey) - _10 >= minSpace ? RE_12357 :
		valToPos(5, scaleKey) - _10 >= minSpace ? RE_125 :
		RE_1
	);

	if (re == RE_1) {
		let magSpace = abs(valToPos(1, scaleKey) - _10);

		if (magSpace < minSpace)
			return _filt(splits.slice().reverse(), sc.distr, re, ceil(minSpace / magSpace)).reverse(); // max->min skip
	}

	return _filt(splits, sc.distr, re, 1);
}

function log2AxisValsFilt(self, splits, axisIdx, foundSpace, foundIncr) {
	let axis = self.axes[axisIdx];
	let scaleKey = axis.scale;
	let minSpace = axis._space;
	let valToPos = self.valToPos;

	let magSpace = abs(valToPos(1, scaleKey) - valToPos(2, scaleKey));

	if (magSpace < minSpace)
		return _filt(splits.slice().reverse(), 3, RE_ALL, ceil(minSpace / magSpace)).reverse(); // max->min skip

	return splits;
}

function numSeriesVal(self, val, seriesIdx, dataIdx) {
	return dataIdx == null ? LEGEND_DISP : val == null ? "" : fmtNum(val);
}

const yAxisOpts = {
	show: true,
	scale: "y",
	stroke: hexBlack,
	space: 30,
	gap: 5,
	alignTo: 1,
	size: 50,
	labelGap: 0,
	labelSize: 30,
	labelFont,
	side: 3,
//	class: "y-vals",
//	incrs: numIncrs,
//	values: (vals, space) => vals,
//	filter: retArg1,
	grid,
	ticks,
	border,
	font,
	lineGap,
	rotate: 0,
};

// takes stroke width
function ptDia(width, mult) {
	let dia = 3 + (width || 1) * 2;
	return roundDec(dia * mult, 3);
}

function seriesPointsShow(self, si) {
	let { scale, idxs } = self.series[0];
	let xData = self._data[0];
	let p0 = self.valToPos(xData[idxs[0]], scale, true);
	let p1 = self.valToPos(xData[idxs[1]], scale, true);
	let dim = abs(p1 - p0);

	let s = self.series[si];
//	const dia = ptDia(s.width, pxRatio);
	let maxPts = dim / (s.points.space * pxRatio);
	return idxs[1] - idxs[0] <= maxPts;
}

const facet = {
	scale: null,
	auto: true,
	sorted: 0,

	// internal caches
	min: inf,
	max: -inf,
};

const gaps = (self, seriesIdx, idx0, idx1, nullGaps) => nullGaps;

const xySeriesOpts = {
	show: true,
	auto: true,
	sorted: 0,
	gaps,
	alpha: 1,
	facets: [
		assign({}, facet, {scale: 'x'}),
		assign({}, facet, {scale: 'y'}),
	],
};

const ySeriesOpts = {
	scale: "y",
	auto: true,
	sorted: 0,
	show: true,
	spanGaps: false,
	gaps,
	alpha: 1,
	points: {
		show: seriesPointsShow,
		filter: null,
	//  paths:
	//	stroke: "#000",
	//	fill: "#fff",
	//	width: 1,
	//	size: 10,
	},
//	label: "Value",
//	value: v => v,
	values: null,

	// internal caches
	min: inf,
	max: -inf,
	idxs: [],

	path: null,
	clip: null,
};

function clampScale(self, val, scaleMin, scaleMax, scaleKey) {
/*
	if (val < 0) {
		let cssHgt = self.bbox.height / pxRatio;
		let absPos = self.valToPos(abs(val), scaleKey);
		let fromBtm = cssHgt - absPos;
		return self.posToVal(cssHgt + fromBtm, scaleKey);
	}
*/
	return scaleMin / 10;
}

const xScaleOpts = {
	time: FEAT_TIME,
	auto: true,
	distr: 1,
	log: 10,
	asinh: 1,
	min: null,
	max: null,
	dir: 1,
	ori: 0,
};

const yScaleOpts = assign({}, xScaleOpts, {
	time: false,
	ori: 1,
});

const syncs = {};

function _sync(key, opts) {
	let s = syncs[key];

	if (!s) {
		s = {
			key,
			plots: [],
			sub(plot) {
				s.plots.push(plot);
			},
			unsub(plot) {
				s.plots = s.plots.filter(c => c != plot);
			},
			pub(type, self, x, y, w, h, i) {
				for (let j = 0; j < s.plots.length; j++)
					s.plots[j] != self && s.plots[j].pub(type, self, x, y, w, h, i);
			},
		};

		if (key != null)
			syncs[key] = s;
	}

	return s;
}

const BAND_CLIP_FILL   = 1 << 0;
const BAND_CLIP_STROKE = 1 << 1;

function orient(u, seriesIdx, cb) {
	const mode = u.mode;
	const series = u.series[seriesIdx];
	const data = mode == 2 ? u._data[seriesIdx] : u._data;
	const scales = u.scales;
	const bbox   = u.bbox;

	let dx = data[0],
		dy = mode == 2 ? data[1] : data[seriesIdx],
		sx = mode == 2 ? scales[series.facets[0].scale] : scales[u.series[0].scale],
		sy = mode == 2 ? scales[series.facets[1].scale] : scales[series.scale],
		l = bbox.left,
		t = bbox.top,
		w = bbox.width,
		h = bbox.height,
		H = u.valToPosH,
		V = u.valToPosV;

	return (sx.ori == 0
		? cb(
			series,
			dx,
			dy,
			sx,
			sy,
			H,
			V,
			l,
			t,
			w,
			h,
			moveToH,
			lineToH,
			rectH,
			arcH,
			bezierCurveToH,
		)
		: cb(
			series,
			dx,
			dy,
			sx,
			sy,
			V,
			H,
			t,
			l,
			h,
			w,
			moveToV,
			lineToV,
			rectV,
			arcV,
			bezierCurveToV,
		)
	);
}

function bandFillClipDirs(self, seriesIdx) {
	let fillDir = 0;

	// 2 bits, -1 | 1
	let clipDirs = 0;

	let bands = ifNull(self.bands, EMPTY_ARR);

	for (let i = 0; i < bands.length; i++) {
		let b = bands[i];

		// is a "from" band edge
		if (b.series[0] == seriesIdx)
			fillDir = b.dir;
		// is a "to" band edge
		else if (b.series[1] == seriesIdx) {
			if (b.dir == 1)
				clipDirs |= 1;
			else
				clipDirs |= 2;
		}
	}

	return [
		fillDir,
		(
			clipDirs == 1 ? -1 : // neg only
			clipDirs == 2 ?  1 : // pos only
			clipDirs == 3 ?  2 : // both
			                 0   // neither
		)
	];
}

function seriesFillTo(self, seriesIdx, dataMin, dataMax, bandFillDir) {
	let mode = self.mode;
	let series = self.series[seriesIdx];
	let scaleKey = mode == 2 ? series.facets[1].scale : series.scale;
	let scale = self.scales[scaleKey];

	return (
		bandFillDir == -1 ? scale.min :
		bandFillDir ==  1 ? scale.max :
		scale.distr ==  3 ? (
			scale.dir == 1 ? scale.min :
			scale.max
		) : 0
	);
}

// creates inverted band clip path (from stroke path -> yMax || yMin)
// clipDir is always inverse of fillDir
// default clip dir is upwards (1), since default band fill is downwards/fillBelowTo (-1) (highIdx -> lowIdx)
function clipBandLine(self, seriesIdx, idx0, idx1, strokePath, clipDir) {
	return orient(self, seriesIdx, (series, dataX, dataY, scaleX, scaleY, valToPosX, valToPosY, xOff, yOff, xDim, yDim) => {
		let pxRound = series.pxRound;

		const dir = scaleX.dir * (scaleX.ori == 0 ? 1 : -1);
		const lineTo = scaleX.ori == 0 ? lineToH : lineToV;

		let frIdx, toIdx;

		if (dir == 1) {
			frIdx = idx0;
			toIdx = idx1;
		}
		else {
			frIdx = idx1;
			toIdx = idx0;
		}

		// path start
		let x0 = pxRound(valToPosX(dataX[frIdx], scaleX, xDim, xOff));
		let y0 = pxRound(valToPosY(dataY[frIdx], scaleY, yDim, yOff));
		// path end x
		let x1 = pxRound(valToPosX(dataX[toIdx], scaleX, xDim, xOff));
		// upper or lower y limit
		let yLimit = pxRound(valToPosY(clipDir == 1 ? scaleY.max : scaleY.min, scaleY, yDim, yOff));

		let clip = new Path2D(strokePath);

		lineTo(clip, x1, yLimit);
		lineTo(clip, x0, yLimit);
		lineTo(clip, x0, y0);

		return clip;
	});
}

function clipGaps(gaps, ori, plotLft, plotTop, plotWid, plotHgt) {
	let clip = null;

	// create clip path (invert gaps and non-gaps)
	if (gaps.length > 0) {
		clip = new Path2D();

		const rect = ori == 0 ? rectH : rectV;

		let prevGapEnd = plotLft;

		for (let i = 0; i < gaps.length; i++) {
			let g = gaps[i];

			if (g[1] > g[0]) {
				let w = g[0] - prevGapEnd;

				w > 0 && rect(clip, prevGapEnd, plotTop, w, plotTop + plotHgt);

				prevGapEnd = g[1];
			}
		}

		let w = plotLft + plotWid - prevGapEnd;

		// hack to ensure we expand the clip enough to avoid cutting off strokes at edges
		let maxStrokeWidth = 10;

		w > 0 && rect(clip, prevGapEnd, plotTop - maxStrokeWidth / 2, w, plotTop + plotHgt + maxStrokeWidth);
	}

	return clip;
}

function addGap(gaps, fromX, toX) {
	let prevGap = gaps[gaps.length - 1];

	if (prevGap && prevGap[0] == fromX)			// TODO: gaps must be encoded at stroke widths?
		prevGap[1] = toX;
	else
		gaps.push([fromX, toX]);
}

function findGaps(xs, ys, idx0, idx1, dir, pixelForX, align) {
	let gaps = [];
	let len = xs.length;

	for (let i = dir == 1 ? idx0 : idx1; i >= idx0 && i <= idx1; i += dir) {
		let yVal = ys[i];

		if (yVal === null) {
			let fr = i, to = i;

			if (dir == 1) {
				while (++i <= idx1 && ys[i] === null)
					to = i;
			}
			else {
				while (--i >= idx0 && ys[i] === null)
					to = i;
			}

			let frPx = pixelForX(xs[fr]);
			let toPx = to == fr ? frPx : pixelForX(xs[to]);

			// if value adjacent to edge null is same pixel, then it's partially
			// filled and gap should start at next pixel
			let fri2 = fr - dir;
			let frPx2 = align <= 0 && fri2 >= 0 && fri2 < len ? pixelForX(xs[fri2]) : frPx;
		//	if (frPx2 == frPx)
		//		frPx++;
		//	else
				frPx = frPx2;

			let toi2 = to + dir;
			let toPx2 = align >= 0 && toi2 >= 0 && toi2 < len ? pixelForX(xs[toi2]) : toPx;
		//	if (toPx2 == toPx)
		//		toPx--;
		//	else
				toPx = toPx2;

			if (toPx >= frPx)
				gaps.push([frPx, toPx]); // addGap
		}
	}

	return gaps;
}

function pxRoundGen(pxAlign) {
	return pxAlign == 0 ? retArg0 : pxAlign == 1 ? round : v => incrRound(v, pxAlign);
}

/*
// inefficient linear interpolation that does bi-directinal scans on each call
export function costlyLerp(i, idx0, idx1, _dirX, dataY) {
	let prevNonNull = nonNullIdx(dataY, _dirX == 1 ? idx0 : idx1, i, -_dirX);
	let nextNonNull = nonNullIdx(dataY, i, _dirX == 1 ? idx1 : idx0,  _dirX);

	let prevVal = dataY[prevNonNull];
	let nextVal = dataY[nextNonNull];

	return prevVal + (i - prevNonNull) / (nextNonNull - prevNonNull) * (nextVal - prevVal);
}
*/

function rect(ori) {
	let moveTo = ori == 0 ?
		moveToH :
		moveToV;

	let arcTo = ori == 0 ?
		(p, x1, y1, x2, y2, r) => { p.arcTo(x1, y1, x2, y2, r); } :
		(p, y1, x1, y2, x2, r) => { p.arcTo(x1, y1, x2, y2, r); };

	let rect = ori == 0 ?
		(p, x, y, w, h) => { p.rect(x, y, w, h); } :
		(p, y, x, h, w) => { p.rect(x, y, w, h); };

	// TODO (pending better browser support): https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/roundRect
	return (p, x, y, w, h, endRad = 0, baseRad = 0) => {
		if (endRad == 0 && baseRad == 0)
			rect(p, x, y, w, h);
		else {
			endRad  = min(endRad,  w / 2, h / 2);
			baseRad = min(baseRad, w / 2, h / 2);

			// adapted from https://stackoverflow.com/questions/1255512/how-to-draw-a-rounded-rectangle-using-html-canvas/7838871#7838871
			moveTo(p, x + endRad, y);
			arcTo(p, x + w, y, x + w, y + h, endRad);
			arcTo(p, x + w, y + h, x, y + h, baseRad);
			arcTo(p, x, y + h, x, y, baseRad);
			arcTo(p, x, y, x + w, y, endRad);
			p.closePath();
		}
	};
}

// orientation-inverting canvas functions
const moveToH = (p, x, y) => { p.moveTo(x, y); };
const moveToV = (p, y, x) => { p.moveTo(x, y); };
const lineToH = (p, x, y) => { p.lineTo(x, y); };
const lineToV = (p, y, x) => { p.lineTo(x, y); };
const rectH = rect(0);
const rectV = rect(1);
const arcH = (p, x, y, r, startAngle, endAngle) => { p.arc(x, y, r, startAngle, endAngle); };
const arcV = (p, y, x, r, startAngle, endAngle) => { p.arc(x, y, r, startAngle, endAngle); };
const bezierCurveToH = (p, bp1x, bp1y, bp2x, bp2y, p2x, p2y) => { p.bezierCurveTo(bp1x, bp1y, bp2x, bp2y, p2x, p2y); };
const bezierCurveToV = (p, bp1y, bp1x, bp2y, bp2x, p2y, p2x) => { p.bezierCurveTo(bp1x, bp1y, bp2x, bp2y, p2x, p2y); };

// TODO: drawWrap(seriesIdx, drawPoints) (save, restore, translate, clip)
function points(opts) {
	return (u, seriesIdx, idx0, idx1, filtIdxs) => {
	//	log("drawPoints()", arguments);

		return orient(u, seriesIdx, (series, dataX, dataY, scaleX, scaleY, valToPosX, valToPosY, xOff, yOff, xDim, yDim) => {
			let { pxRound, points } = series;

			let moveTo, arc;

			if (scaleX.ori == 0) {
				moveTo = moveToH;
				arc = arcH;
			}
			else {
				moveTo = moveToV;
				arc = arcV;
			}

			const width = roundDec(points.width * pxRatio, 3);

			let rad = (points.size - points.width) / 2 * pxRatio;
			let dia = roundDec(rad * 2, 3);

			let fill = new Path2D();
			let clip = new Path2D();

			let { left: lft, top: top, width: wid, height: hgt } = u.bbox;

			rectH(clip,
				lft - dia,
				top - dia,
				wid + dia * 2,
				hgt + dia * 2,
			);

			const drawPoint = pi => {
				if (dataY[pi] != null) {
					let x = pxRound(valToPosX(dataX[pi], scaleX, xDim, xOff));
					let y = pxRound(valToPosY(dataY[pi], scaleY, yDim, yOff));

					moveTo(fill, x + rad, y);
					arc(fill, x, y, rad, 0, PI * 2);
				}
			};

			if (filtIdxs)
				filtIdxs.forEach(drawPoint);
			else {
				for (let pi = idx0; pi <= idx1; pi++)
					drawPoint(pi);
			}

			return {
				stroke: width > 0 ? fill : null,
				fill,
				clip,
				flags: BAND_CLIP_FILL | BAND_CLIP_STROKE,
			};
		});
	};
}

function _drawAcc(lineTo) {
	return (stroke, accX, minY, maxY, inY, outY) => {
		if (minY != maxY) {
			if (inY != minY && outY != minY)
				lineTo(stroke, accX, minY);
			if (inY != maxY && outY != maxY)
				lineTo(stroke, accX, maxY);

			lineTo(stroke, accX, outY);
		}
	};
}

const drawAccH = _drawAcc(lineToH);
const drawAccV = _drawAcc(lineToV);

function linear(opts) {
	const alignGaps = ifNull(opts?.alignGaps, 0);

	return (u, seriesIdx, idx0, idx1) => {
		return orient(u, seriesIdx, (series, dataX, dataY, scaleX, scaleY, valToPosX, valToPosY, xOff, yOff, xDim, yDim) => {
			[idx0, idx1] = nonNullIdxs(dataY, idx0, idx1);

			let pxRound = series.pxRound;

			let pixelForX = val => pxRound(valToPosX(val, scaleX, xDim, xOff));
			let pixelForY = val => pxRound(valToPosY(val, scaleY, yDim, yOff));

			let lineTo, drawAcc;

			if (scaleX.ori == 0) {
				lineTo = lineToH;
				drawAcc = drawAccH;
			}
			else {
				lineTo = lineToV;
				drawAcc = drawAccV;
			}

			const dir = scaleX.dir * (scaleX.ori == 0 ? 1 : -1);

			const _paths = {stroke: new Path2D(), fill: null, clip: null, band: null, gaps: null, flags: BAND_CLIP_FILL};
			const stroke = _paths.stroke;

			let hasGap = false;

			// decimate when number of points >= 4x available pixels
			const decimate = idx1 - idx0 >= xDim * 4;

			if (decimate) {
				let xForPixel = pos => u.posToVal(pos, scaleX.key, true);

				let minY = null,
					maxY = null,
					inY, outY, drawnAtX;

				let accX = pixelForX(dataX[dir == 1 ? idx0 : idx1]);

				let idx0px = pixelForX(dataX[idx0]);
				let idx1px = pixelForX(dataX[idx1]);

				// tracks limit of current x bucket to avoid having to get x pixel for every x value
				let nextAccXVal = xForPixel(dir == 1 ? idx0px + 1 : idx1px - 1);

				for (let i = dir == 1 ? idx0 : idx1; i >= idx0 && i <= idx1; i += dir) {
					let xVal = dataX[i];
					let reuseAccX = dir == 1 ? (xVal < nextAccXVal) : (xVal > nextAccXVal);
					let x = reuseAccX ? accX :  pixelForX(xVal);

					let yVal = dataY[i];

					if (x == accX) {
						if (yVal != null) {
							outY = yVal;

							if (minY == null) {
								lineTo(stroke, x, pixelForY(outY));
								inY = minY = maxY = outY;
							} else {
								if (outY < minY)
									minY = outY;
								else if (outY > maxY)
									maxY = outY;
							}
						}
						else {
							if (yVal === null)
								hasGap = true;
						}
					}
					else {
						if (minY != null)
							drawAcc(stroke, accX, pixelForY(minY), pixelForY(maxY), pixelForY(inY), pixelForY(outY));

						if (yVal != null) {
							outY = yVal;
							lineTo(stroke, x, pixelForY(outY));
							minY = maxY = inY = outY;
						}
						else {
							minY = maxY = null;

							if (yVal === null)
								hasGap = true;
						}

						accX = x;
						nextAccXVal = xForPixel(accX + dir);
					}
				}

				if (minY != null && minY != maxY && drawnAtX != accX)
					drawAcc(stroke, accX, pixelForY(minY), pixelForY(maxY), pixelForY(inY), pixelForY(outY));
			}
			else {
				for (let i = dir == 1 ? idx0 : idx1; i >= idx0 && i <= idx1; i += dir) {
					let yVal = dataY[i];

					if (yVal === null)
						hasGap = true;
					else if (yVal != null)
						lineTo(stroke, pixelForX(dataX[i]), pixelForY(yVal));
				}
			}

			let [ bandFillDir, bandClipDir ] = bandFillClipDirs(u, seriesIdx);

			if (series.fill != null || bandFillDir != 0) {
				let fill = _paths.fill = new Path2D(stroke);

				let fillToVal = series.fillTo(u, seriesIdx, series.min, series.max, bandFillDir);
				let fillToY = pixelForY(fillToVal);

				let frX = pixelForX(dataX[idx0]);
				let toX = pixelForX(dataX[idx1]);

				if (dir == -1)
					[toX, frX] = [frX, toX];

				lineTo(fill, toX, fillToY);
				lineTo(fill, frX, fillToY);
			}

			if (!series.spanGaps) { // skip in mode: 2?
			//	console.time('gaps');
				let gaps = [];

				hasGap && gaps.push(...findGaps(dataX, dataY, idx0, idx1, dir, pixelForX, alignGaps));

			//	console.timeEnd('gaps');

			//	console.log('gaps', JSON.stringify(gaps));

				_paths.gaps = gaps = series.gaps(u, seriesIdx, idx0, idx1, gaps);

				_paths.clip = clipGaps(gaps, scaleX.ori, xOff, yOff, xDim, yDim);
			}

			if (bandClipDir != 0) {
				_paths.band = bandClipDir == 2 ? [
					clipBandLine(u, seriesIdx, idx0, idx1, stroke, -1),
					clipBandLine(u, seriesIdx, idx0, idx1, stroke,  1),
				] : clipBandLine(u, seriesIdx, idx0, idx1, stroke, bandClipDir);
			}

			return _paths;
		});
	};
}

// BUG: align: -1 behaves like align: 1 when scale.dir: -1
function stepped(opts) {
	const align = ifNull(opts.align, 1);
	// whether to draw ascenders/descenders at null/gap bondaries
	const ascDesc = ifNull(opts.ascDesc, false);
	const alignGaps = ifNull(opts.alignGaps, 0);
	const extend = ifNull(opts.extend, false);

	return (u, seriesIdx, idx0, idx1) => {
		return orient(u, seriesIdx, (series, dataX, dataY, scaleX, scaleY, valToPosX, valToPosY, xOff, yOff, xDim, yDim) => {
			[idx0, idx1] = nonNullIdxs(dataY, idx0, idx1);

			let pxRound = series.pxRound;

			let { left, width } = u.bbox;

			let pixelForX = val => pxRound(valToPosX(val, scaleX, xDim, xOff));
			let pixelForY = val => pxRound(valToPosY(val, scaleY, yDim, yOff));

			let lineTo = scaleX.ori == 0 ? lineToH : lineToV;

			const _paths = {stroke: new Path2D(), fill: null, clip: null, band: null, gaps: null, flags: BAND_CLIP_FILL};
			const stroke = _paths.stroke;

			const dir = scaleX.dir * (scaleX.ori == 0 ? 1 : -1);

			let prevYPos  = pixelForY(dataY[dir == 1 ? idx0 : idx1]);
			let firstXPos = pixelForX(dataX[dir == 1 ? idx0 : idx1]);
			let prevXPos = firstXPos;

			let firstXPosExt = firstXPos;

			if (extend && align == -1) {
				firstXPosExt = left;
				lineTo(stroke, firstXPosExt, prevYPos);
			}

			lineTo(stroke, firstXPos, prevYPos);

			for (let i = dir == 1 ? idx0 : idx1; i >= idx0 && i <= idx1; i += dir) {
				let yVal1 = dataY[i];

				if (yVal1 == null)
					continue;

				let x1 = pixelForX(dataX[i]);
				let y1 = pixelForY(yVal1);

				if (align == 1)
					lineTo(stroke, x1, prevYPos);
				else
					lineTo(stroke, prevXPos, y1);

				lineTo(stroke, x1, y1);

				prevYPos = y1;
				prevXPos = x1;
			}

			let prevXPosExt = prevXPos;

			if (extend && align == 1) {
				prevXPosExt = left + width;
				lineTo(stroke, prevXPosExt, prevYPos);
			}

			let [ bandFillDir, bandClipDir ] = bandFillClipDirs(u, seriesIdx);

			if (series.fill != null || bandFillDir != 0) {
				let fill = _paths.fill = new Path2D(stroke);

				let fillTo = series.fillTo(u, seriesIdx, series.min, series.max, bandFillDir);
				let fillToY = pixelForY(fillTo);

				lineTo(fill, prevXPosExt, fillToY);
				lineTo(fill, firstXPosExt, fillToY);
			}

			if (!series.spanGaps) {
			//	console.time('gaps');
				let gaps = [];

				gaps.push(...findGaps(dataX, dataY, idx0, idx1, dir, pixelForX, alignGaps));

			//	console.timeEnd('gaps');

			//	console.log('gaps', JSON.stringify(gaps));

				// expand/contract clips for ascenders/descenders
				let halfStroke = (series.width * pxRatio) / 2;
				let startsOffset = (ascDesc || align ==  1) ?  halfStroke : -halfStroke;
				let endsOffset   = (ascDesc || align == -1) ? -halfStroke :  halfStroke;

				gaps.forEach(g => {
					g[0] += startsOffset;
					g[1] += endsOffset;
				});

				_paths.gaps = gaps = series.gaps(u, seriesIdx, idx0, idx1, gaps);

				_paths.clip = clipGaps(gaps, scaleX.ori, xOff, yOff, xDim, yDim);
			}

			if (bandClipDir != 0) {
				_paths.band = bandClipDir == 2 ? [
					clipBandLine(u, seriesIdx, idx0, idx1, stroke, -1),
					clipBandLine(u, seriesIdx, idx0, idx1, stroke,  1),
				] : clipBandLine(u, seriesIdx, idx0, idx1, stroke, bandClipDir);
			}

			return _paths;
		});
	};
}

function findColWidth(dataX, dataY, valToPosX, scaleX, xDim, xOff, colWid = inf) {
	if (dataX.length > 1) {
		// prior index with non-undefined y data
		let prevIdx = null;

		// scan full dataset for smallest adjacent delta
		// will not work properly for non-linear x scales, since does not do expensive valToPosX calcs till end
		for (let i = 0, minDelta = Infinity; i < dataX.length; i++) {
			if (dataY[i] !== undefined) {
				if (prevIdx != null) {
					let delta = abs(dataX[i] - dataX[prevIdx]);

					if (delta < minDelta) {
						minDelta = delta;
						colWid = abs(valToPosX(dataX[i], scaleX, xDim, xOff) - valToPosX(dataX[prevIdx], scaleX, xDim, xOff));
					}
				}

				prevIdx = i;
			}
		}
	}

	return colWid;
}

function bars(opts) {
	opts = opts || EMPTY_OBJ;
	const size = ifNull(opts.size, [0.6, inf, 1]);
	const align = opts.align || 0;
	const _extraGap = (opts.gap || 0);

	let ro = opts.radius;

	ro =
		// [valueRadius, baselineRadius]
		ro == null ? [0, 0] :
		typeof ro == 'number' ? [ro, 0] : ro;

	const radiusFn = fnOrSelf(ro);

	const gapFactor = 1 - size[0];
	const _maxWidth  = ifNull(size[1], inf);
	const _minWidth  = ifNull(size[2], 1);

	const disp = ifNull(opts.disp, EMPTY_OBJ);
	const _each = ifNull(opts.each, _ => {});

	const { fill: dispFills, stroke: dispStrokes } = disp;

	return (u, seriesIdx, idx0, idx1) => {
		return orient(u, seriesIdx, (series, dataX, dataY, scaleX, scaleY, valToPosX, valToPosY, xOff, yOff, xDim, yDim) => {
			let pxRound = series.pxRound;
			let _align = align;

			let extraGap = _extraGap * pxRatio;
			let maxWidth = _maxWidth * pxRatio;
			let minWidth = _minWidth * pxRatio;

			let valRadius, baseRadius;

			if (scaleX.ori == 0)
				[valRadius, baseRadius] = radiusFn(u, seriesIdx);
			else
				[baseRadius, valRadius] = radiusFn(u, seriesIdx);

			const _dirX = scaleX.dir * (scaleX.ori == 0 ? 1 : -1);
		//	const _dirY = scaleY.dir * (scaleY.ori == 1 ? 1 : -1);

			let rect = scaleX.ori == 0 ? rectH : rectV;

			let each = scaleX.ori == 0 ? _each : (u, seriesIdx, i, top, lft, hgt, wid) => {
				_each(u, seriesIdx, i, lft, top, wid, hgt);
			};

			// band where this series is the "from" edge
			let band = ifNull(u.bands, EMPTY_ARR).find(b => b.series[0] == seriesIdx);

			let fillDir = band != null ? band.dir : 0;
			let fillTo = series.fillTo(u, seriesIdx, series.min, series.max, fillDir);
			let fillToY = pxRound(valToPosY(fillTo, scaleY, yDim, yOff));

			// barWid is to center of stroke
			let xShift, barWid, fullGap, colWid = xDim;

			let strokeWidth = pxRound(series.width * pxRatio);

			let multiPath = false;

			let fillColors = null;
			let fillPaths = null;
			let strokeColors = null;
			let strokePaths = null;

			if (dispFills != null && (strokeWidth == 0 || dispStrokes != null)) {
				multiPath = true;

				fillColors = dispFills.values(u, seriesIdx, idx0, idx1);
				fillPaths = new Map();
				(new Set(fillColors)).forEach(color => {
					if (color != null)
						fillPaths.set(color, new Path2D());
				});

				if (strokeWidth > 0) {
					strokeColors = dispStrokes.values(u, seriesIdx, idx0, idx1);
					strokePaths = new Map();
					(new Set(strokeColors)).forEach(color => {
						if (color != null)
							strokePaths.set(color, new Path2D());
					});
				}
			}

			let { x0, size } = disp;

			if (x0 != null && size != null) {
				_align = 1;
				dataX = x0.values(u, seriesIdx, idx0, idx1);

				if (x0.unit == 2)
					dataX = dataX.map(pct => u.posToVal(xOff + pct * xDim, scaleX.key, true));

				// assumes uniform sizes, for now
				let sizes = size.values(u, seriesIdx, idx0, idx1);

				if (size.unit == 2)
					barWid = sizes[0] * xDim;
				else
					barWid = valToPosX(sizes[0], scaleX, xDim, xOff) - valToPosX(0, scaleX, xDim, xOff); // assumes linear scale (delta from 0)

				colWid = findColWidth(dataX, dataY, valToPosX, scaleX, xDim, xOff, colWid);

				let gapWid = colWid - barWid;
				fullGap = gapWid + extraGap;
			}
			else {
				colWid = findColWidth(dataX, dataY, valToPosX, scaleX, xDim, xOff, colWid);

				let gapWid = colWid * gapFactor;

				fullGap = gapWid + extraGap;
				barWid = colWid - fullGap;
			}

			if (fullGap < 1)
				fullGap = 0;

			if (strokeWidth >= barWid / 2)
				strokeWidth = 0;

			// for small gaps, disable pixel snapping since gap inconsistencies become noticible and annoying
			if (fullGap < 5)
				pxRound = retArg0;

			let insetStroke = fullGap > 0;

			let rawBarWid = colWid - fullGap - (insetStroke ? strokeWidth : 0);

			barWid = pxRound(clamp(rawBarWid, minWidth, maxWidth));

			xShift = (_align == 0 ? barWid / 2 : _align == _dirX ? 0 : barWid) - _align * _dirX * ((_align == 0 ? extraGap / 2 : 0) + (insetStroke ? strokeWidth / 2 : 0));


			const _paths = {stroke: null, fill: null, clip: null, band: null, gaps: null, flags: 0};  // disp, geom

			const stroke = multiPath ? null : new Path2D();

			let dataY0 = null;

			if (band != null)
				dataY0 = u.data[band.series[1]];
			else {
				let { y0, y1 } = disp;

				if (y0 != null && y1 != null) {
					dataY = y1.values(u, seriesIdx, idx0, idx1);
					dataY0 = y0.values(u, seriesIdx, idx0, idx1);
				}
			}

			let radVal = valRadius * barWid;
			let radBase = baseRadius * barWid;

			for (let i = _dirX == 1 ? idx0 : idx1; i >= idx0 && i <= idx1; i += _dirX) {
				let yVal = dataY[i];

				if (yVal == null)
					continue;

				if (dataY0 != null) {
					let yVal0 = dataY0[i] ?? 0;

					if (yVal - yVal0 == 0)
						continue;

					fillToY = valToPosY(yVal0, scaleY, yDim, yOff);
				}

				let xVal = scaleX.distr != 2 || disp != null ? dataX[i] : i;

				// TODO: all xPos can be pre-computed once for all series in aligned set
				let xPos = valToPosX(xVal, scaleX, xDim, xOff);
				let yPos = valToPosY(ifNull(yVal, fillTo), scaleY, yDim, yOff);

				let lft = pxRound(xPos - xShift);
				let btm = pxRound(max(yPos, fillToY));
				let top = pxRound(min(yPos, fillToY));
				// this includes the stroke
				let barHgt = btm - top;

				if (yVal != null) {  // && yVal != fillTo (0 height bar)
					let rv = yVal < 0 ? radBase : radVal;
					let rb = yVal < 0 ? radVal : radBase;

					if (multiPath) {
						if (strokeWidth > 0 && strokeColors[i] != null)
							rect(strokePaths.get(strokeColors[i]), lft, top + floor(strokeWidth / 2), barWid, max(0, barHgt - strokeWidth), rv, rb);

						if (fillColors[i] != null)
							rect(fillPaths.get(fillColors[i]), lft, top + floor(strokeWidth / 2), barWid, max(0, barHgt - strokeWidth), rv, rb);
					}
					else
						rect(stroke, lft, top + floor(strokeWidth / 2), barWid, max(0, barHgt - strokeWidth), rv, rb);

					each(u, seriesIdx, i,
						lft    - strokeWidth / 2,
						top,
						barWid + strokeWidth,
						barHgt,
					);
				}
			}

			if (strokeWidth > 0)
				_paths.stroke = multiPath ? strokePaths : stroke;
			else if (!multiPath) {
				_paths._fill = series.width == 0 ? series._fill : series._stroke ?? series._fill;
				_paths.width = 0;
			}

			_paths.fill = multiPath ? fillPaths : stroke;

			return _paths;
		});
	};
}

function splineInterp(interp, opts) {
	const alignGaps = ifNull(opts?.alignGaps, 0);

	return (u, seriesIdx, idx0, idx1) => {
		return orient(u, seriesIdx, (series, dataX, dataY, scaleX, scaleY, valToPosX, valToPosY, xOff, yOff, xDim, yDim) => {
			[idx0, idx1] = nonNullIdxs(dataY, idx0, idx1);

			let pxRound = series.pxRound;

			let pixelForX = val => pxRound(valToPosX(val, scaleX, xDim, xOff));
			let pixelForY = val => pxRound(valToPosY(val, scaleY, yDim, yOff));

			let moveTo, bezierCurveTo, lineTo;

			if (scaleX.ori == 0) {
				moveTo = moveToH;
				lineTo = lineToH;
				bezierCurveTo = bezierCurveToH;
			}
			else {
				moveTo = moveToV;
				lineTo = lineToV;
				bezierCurveTo = bezierCurveToV;
			}

			const dir = scaleX.dir * (scaleX.ori == 0 ? 1 : -1);

			let firstXPos = pixelForX(dataX[dir == 1 ? idx0 : idx1]);
			let prevXPos = firstXPos;

			let xCoords = [];
			let yCoords = [];

			for (let i = dir == 1 ? idx0 : idx1; i >= idx0 && i <= idx1; i += dir) {
				let yVal = dataY[i];

				if (yVal != null) {
					let xVal = dataX[i];
					let xPos = pixelForX(xVal);

					xCoords.push(prevXPos = xPos);
					yCoords.push(pixelForY(dataY[i]));
				}
			}

			const _paths = {stroke: interp(xCoords, yCoords, moveTo, lineTo, bezierCurveTo, pxRound), fill: null, clip: null, band: null, gaps: null, flags: BAND_CLIP_FILL};
			const stroke = _paths.stroke;

			let [ bandFillDir, bandClipDir ] = bandFillClipDirs(u, seriesIdx);

			if (series.fill != null || bandFillDir != 0) {
				let fill = _paths.fill = new Path2D(stroke);

				let fillTo = series.fillTo(u, seriesIdx, series.min, series.max, bandFillDir);
				let fillToY = pixelForY(fillTo);

				lineTo(fill, prevXPos, fillToY);
				lineTo(fill, firstXPos, fillToY);
			}

			if (!series.spanGaps) {
			//	console.time('gaps');
				let gaps = [];

				gaps.push(...findGaps(dataX, dataY, idx0, idx1, dir, pixelForX, alignGaps));

			//	console.timeEnd('gaps');

			//	console.log('gaps', JSON.stringify(gaps));

				_paths.gaps = gaps = series.gaps(u, seriesIdx, idx0, idx1, gaps);

				_paths.clip = clipGaps(gaps, scaleX.ori, xOff, yOff, xDim, yDim);
			}

			if (bandClipDir != 0) {
				_paths.band = bandClipDir == 2 ? [
					clipBandLine(u, seriesIdx, idx0, idx1, stroke, -1),
					clipBandLine(u, seriesIdx, idx0, idx1, stroke,  1),
				] : clipBandLine(u, seriesIdx, idx0, idx1, stroke, bandClipDir);
			}

			return _paths;

			//  if FEAT_PATHS: false in rollup.config.js
			//	u.ctx.save();
			//	u.ctx.beginPath();
			//	u.ctx.rect(u.bbox.left, u.bbox.top, u.bbox.width, u.bbox.height);
			//	u.ctx.clip();
			//	u.ctx.strokeStyle = u.series[sidx].stroke;
			//	u.ctx.stroke(stroke);
			//	u.ctx.fillStyle = u.series[sidx].fill;
			//	u.ctx.fill(fill);
			//	u.ctx.restore();
			//	return null;
		});
	};
}

function monotoneCubic(opts) {
	return splineInterp(_monotoneCubic, opts);
}

// Monotone Cubic Spline interpolation, adapted from the Chartist.js implementation:
// https://github.com/gionkunz/chartist-js/blob/e7e78201bffe9609915e5e53cfafa29a5d6c49f9/src/scripts/interpolation.js#L240-L369
function _monotoneCubic(xs, ys, moveTo, lineTo, bezierCurveTo, pxRound) {
	const n = xs.length;

	if (n < 2)
		return null;

	const path = new Path2D();

	moveTo(path, xs[0], ys[0]);

	if (n == 2)
		lineTo(path, xs[1], ys[1]);
	else {
		let ms  = Array(n),
			ds  = Array(n - 1),
			dys = Array(n - 1),
			dxs = Array(n - 1);

		// calc deltas and derivative
		for (let i = 0; i < n - 1; i++) {
			dys[i] = ys[i + 1] - ys[i];
			dxs[i] = xs[i + 1] - xs[i];
			ds[i]  = dys[i] / dxs[i];
		}

		// determine desired slope (m) at each point using Fritsch-Carlson method
		// http://math.stackexchange.com/questions/45218/implementation-of-monotone-cubic-interpolation
		ms[0] = ds[0];

		for (let i = 1; i < n - 1; i++) {
			if (ds[i] === 0 || ds[i - 1] === 0 || (ds[i - 1] > 0) !== (ds[i] > 0))
				ms[i] = 0;
			else {
				ms[i] = 3 * (dxs[i - 1] + dxs[i]) / (
					(2 * dxs[i] + dxs[i - 1]) / ds[i - 1] +
					(dxs[i] + 2 * dxs[i - 1]) / ds[i]
				);

				if (!isFinite(ms[i]))
					ms[i] = 0;
			}
		}

		ms[n - 1] = ds[n - 2];

		for (let i = 0; i < n - 1; i++) {
			bezierCurveTo(
				path,
				xs[i] + dxs[i] / 3,
				ys[i] + ms[i] * dxs[i] / 3,
				xs[i + 1] - dxs[i] / 3,
				ys[i + 1] - ms[i + 1] * dxs[i] / 3,
				xs[i + 1],
				ys[i + 1],
			);
		}
	}

	return path;
}

const cursorPlots = new Set();

function invalidateRects() {
	for (let u of cursorPlots)
		u.syncRect(true);
}

if (domEnv) {
	on(resize, win, invalidateRects);
	on(scroll, win, invalidateRects, true);
	on(dppxchange, win, () => { uPlot.pxRatio = pxRatio; });
}

const linearPath = linear() ;
const pointsPath = points() ;

function setDefaults(d, xo, yo, initY) {
	let d2 = initY ? [d[0], d[1]].concat(d.slice(2)) : [d[0]].concat(d.slice(1));
	return d2.map((o, i) => setDefault(o, i, xo, yo));
}

function setDefaults2(d, xyo) {
	return d.map((o, i) => i == 0 ? {} : assign({}, xyo, o));  // todo: assign() will not merge facet arrays
}

function setDefault(o, i, xo, yo) {
	return assign({}, (i == 0 ? xo : yo), o);
}

function snapNumX(self, dataMin, dataMax) {
	return dataMin == null ? nullNullTuple : [dataMin, dataMax];
}

const snapTimeX = snapNumX;

// this ensures that non-temporal/numeric y-axes get multiple-snapped padding added above/below
// TODO: also account for incrs when snapping to ensure top of axis gets a tick & value
function snapNumY(self, dataMin, dataMax) {
	return dataMin == null ? nullNullTuple : rangeNum(dataMin, dataMax, rangePad, true);
}

function snapLogY(self, dataMin, dataMax, scale) {
	return dataMin == null ? nullNullTuple : rangeLog(dataMin, dataMax, self.scales[scale].log, false);
}

const snapLogX = snapLogY;

function snapAsinhY(self, dataMin, dataMax, scale) {
	return dataMin == null ? nullNullTuple : rangeAsinh(dataMin, dataMax, self.scales[scale].log, false);
}

const snapAsinhX = snapAsinhY;

// dim is logical (getClientBoundingRect) pixels, not canvas pixels
function findIncr(minVal, maxVal, incrs, dim, minSpace) {
	let intDigits = max(numIntDigits(minVal), numIntDigits(maxVal));

	let delta = maxVal - minVal;

	let incrIdx = closestIdx((minSpace / dim) * delta, incrs);

	do {
		let foundIncr = incrs[incrIdx];
		let foundSpace = dim * foundIncr / delta;

		if (foundSpace >= minSpace && intDigits + (foundIncr < 5 ? fixedDec.get(foundIncr) : 0) <= 17)
			return [foundIncr, foundSpace];
	} while (++incrIdx < incrs.length);

	return [0, 0];
}

function pxRatioFont(font) {
	let fontSize, fontSizeCss;
	font = font.replace(/(\d+)px/, (m, p1) => (fontSize = round((fontSizeCss = +p1) * pxRatio)) + 'px');
	return [font, fontSize, fontSizeCss];
}

function syncFontSize(axis) {
	if (axis.show) {
		[axis.font, axis.labelFont].forEach(f => {
			let size = roundDec(f[2] * pxRatio, 1);
			f[0] = f[0].replace(/[0-9.]+px/, size + 'px');
			f[1] = size;
		});
	}
}

function uPlot(opts, data, then) {
	const self = {
		mode: ifNull(opts.mode, 1),
	};

	const mode = self.mode;

	function getHPos(val, scale, dim, off) {
		let pct = scale.valToPct(val);
		return off + dim * (scale.dir == -1 ? (1 - pct) : pct);
	}

	function getVPos(val, scale, dim, off) {
		let pct = scale.valToPct(val);
		return off + dim * (scale.dir == -1 ? pct : (1 - pct));
	}

	function getPos(val, scale, dim, off) {
		return scale.ori == 0 ? getHPos(val, scale, dim, off) : getVPos(val, scale, dim, off);
	}

	self.valToPosH = getHPos;
	self.valToPosV = getVPos;

	let ready = false;
	self.status = 0;

	const root = self.root = placeDiv(UPLOT);

	if (opts.id != null)
		root.id = opts.id;

	addClass(root, opts.class);

	if (opts.title) {
		let title = placeDiv(TITLE, root);
		title.textContent = opts.title;
	}

	const can = placeTag("canvas");
	const ctx = self.ctx = can.getContext("2d");

	const wrap = placeDiv(WRAP, root);

	on("click", wrap, e => {
		if (e.target === over) {
			let didDrag = mouseLeft1 != mouseLeft0 || mouseTop1 != mouseTop0;
			didDrag && drag.click(self, e);
		}
	}, true);

	const under = self.under = placeDiv(UNDER, wrap);
	wrap.appendChild(can);
	const over = self.over = placeDiv(OVER, wrap);

	opts = copy(opts);

	const pxAlign = +ifNull(opts.pxAlign, 1);

	const pxRound = pxRoundGen(pxAlign);

	(opts.plugins || []).forEach(p => {
		if (p.opts)
			opts = p.opts(self, opts) || opts;
	});

	const ms = opts.ms || 1e-3;

	const series  = self.series = mode == 1 ?
		setDefaults(opts.series || [], xSeriesOpts, ySeriesOpts, false) :
		setDefaults2(opts.series || [null], xySeriesOpts);
	const axes    = self.axes   = setDefaults(opts.axes   || [], xAxisOpts,   yAxisOpts,    true);
	const scales  = self.scales = {};
	const bands   = self.bands  = opts.bands || [];

	bands.forEach(b => {
		b.fill = fnOrSelf(b.fill || null);
		b.dir = ifNull(b.dir, -1);
	});

	const xScaleKey = mode == 2 ? series[1].facets[0].scale : series[0].scale;

	const drawOrderMap = {
		axes: drawAxesGrid,
		series: drawSeries,
	};

	const drawOrder = (opts.drawOrder || ["axes", "series"]).map(key => drawOrderMap[key]);

	function initValToPct(sc) {
		const getVal = (
			sc.distr == 3   ? val => log10(val > 0 ? val : sc.clamp(self, val, sc.min, sc.max, sc.key)) :
			sc.distr == 4   ? val => asinh(val, sc.asinh) :
			sc.distr == 100 ? val => sc.fwd(val) :
			val => val
		);

		return val => {
			let _val = getVal(val);
			let { _min, _max } = sc;
			let delta = _max - _min;
			return (_val - _min) / delta;
		};
	}

	function initScale(scaleKey) {
		let sc = scales[scaleKey];

		if (sc == null) {
			let scaleOpts = (opts.scales || EMPTY_OBJ)[scaleKey] || EMPTY_OBJ;

			if (scaleOpts.from != null) {
				// ensure parent is initialized
				initScale(scaleOpts.from);
				// dependent scales inherit
				let sc = assign({}, scales[scaleOpts.from], scaleOpts, {key: scaleKey});
				sc.valToPct = initValToPct(sc);
				scales[scaleKey] = sc;
			}
			else {
				sc = scales[scaleKey] = assign({}, (scaleKey == xScaleKey ? xScaleOpts : yScaleOpts), scaleOpts);

				sc.key = scaleKey;

				let isTime = sc.time;

				let rn = sc.range;

				let rangeIsArr = isArr(rn);

				if (scaleKey != xScaleKey || (mode == 2 && !isTime)) {
					// if range array has null limits, it should be auto
					if (rangeIsArr && (rn[0] == null || rn[1] == null)) {
						rn = {
							min: rn[0] == null ? autoRangePart : {
								mode: 1,
								hard: rn[0],
								soft: rn[0],
							},
							max: rn[1] == null ? autoRangePart : {
								mode: 1,
								hard: rn[1],
								soft: rn[1],
							},
						};
						rangeIsArr = false;
					}

					if (!rangeIsArr && isObj(rn)) {
						let cfg = rn;
						// this is similar to snapNumY
						rn = (self, dataMin, dataMax) => dataMin == null ? nullNullTuple : rangeNum(dataMin, dataMax, cfg);
					}
				}

				sc.range = fnOrSelf(rn || (isTime ? snapTimeX : scaleKey == xScaleKey ?
					(sc.distr == 3 ? snapLogX : sc.distr == 4 ? snapAsinhX : snapNumX) :
					(sc.distr == 3 ? snapLogY : sc.distr == 4 ? snapAsinhY : snapNumY)
				));

				sc.auto = fnOrSelf(rangeIsArr ? false : sc.auto);

				sc.clamp = fnOrSelf(sc.clamp || clampScale);

				// caches for expensive ops like asinh() & log()
				sc._min = sc._max = null;

				sc.valToPct = initValToPct(sc);
			}
		}
	}

	initScale("x");
	initScale("y");

	// TODO: init scales from facets in mode: 2
	if (mode == 1) {
		series.forEach(s => {
			initScale(s.scale);
		});
	}

	axes.forEach(a => {
		initScale(a.scale);
	});

	for (let k in opts.scales)
		initScale(k);

	const scaleX = scales[xScaleKey];

	const xScaleDistr = scaleX.distr;

	let valToPosX, valToPosY;

	if (scaleX.ori == 0) {
		addClass(root, ORI_HZ);
		valToPosX = getHPos;
		valToPosY = getVPos;
		/*
		updOriDims = () => {
			xDimCan = plotWid;
			xOffCan = plotLft;
			yDimCan = plotHgt;
			yOffCan = plotTop;

			xDimCss = plotWidCss;
			xOffCss = plotLftCss;
			yDimCss = plotHgtCss;
			yOffCss = plotTopCss;
		};
		*/
	}
	else {
		addClass(root, ORI_VT);
		valToPosX = getVPos;
		valToPosY = getHPos;
		/*
		updOriDims = () => {
			xDimCan = plotHgt;
			xOffCan = plotTop;
			yDimCan = plotWid;
			yOffCan = plotLft;

			xDimCss = plotHgtCss;
			xOffCss = plotTopCss;
			yDimCss = plotWidCss;
			yOffCss = plotLftCss;
		};
		*/
	}

	const pendScales = {};

	// explicitly-set initial scales
	for (let k in scales) {
		let sc = scales[k];

		if (sc.min != null || sc.max != null) {
			pendScales[k] = {min: sc.min, max: sc.max};
			sc.min = sc.max = null;
		}
	}

//	self.tz = opts.tz || Intl.DateTimeFormat().resolvedOptions().timeZone;
	const _tzDate  = (opts.tzDate || (ts => new Date(round(ts / ms))));
	const _fmtDate = (opts.fmtDate || fmtDate);

	const _timeAxisSplits = (ms == 1 ? timeAxisSplitsMs(_tzDate) : timeAxisSplitsS(_tzDate));
	const _timeAxisVals   = timeAxisVals(_tzDate, timeAxisStamps((ms == 1 ? _timeAxisStampsMs : _timeAxisStampsS), _fmtDate));
	const _timeSeriesVal  = timeSeriesVal(_tzDate, timeSeriesStamp(_timeSeriesStamp, _fmtDate));

	const activeIdxs = [];

	const legend     = (self.legend = assign({}, legendOpts, opts.legend));
	const cursor     =                (self.cursor = assign({}, cursorOpts, {drag: {y: mode == 2}}, opts.cursor));
	const showLegend = legend.show;
	const showCursor =                cursor.show;
	const markers    = legend.markers;

	{
		legend.idxs = activeIdxs;

		markers.width  = fnOrSelf(markers.width);
		markers.dash   = fnOrSelf(markers.dash);
		markers.stroke = fnOrSelf(markers.stroke);
		markers.fill   = fnOrSelf(markers.fill);
	}

	let legendTable;
	let legendHead;
	let legendBody;
	let legendRows = [];
	let legendCells = [];
	let legendCols;
	let multiValLegend = false;
	let NULL_LEGEND_VALUES = {};

	if (legend.live) {
		const getMultiVals = series[1] ? series[1].values : null;
		multiValLegend = getMultiVals != null;
		legendCols = multiValLegend ? getMultiVals(self, 1, 0) : {_: 0};

		for (let k in legendCols)
			NULL_LEGEND_VALUES[k] = LEGEND_DISP;
	}

	if (showLegend) {
		legendTable = placeTag("table", LEGEND, root);
		legendBody = placeTag("tbody", null, legendTable);

		// allows legend to be moved out of root
		legend.mount(self, legendTable);

		if (multiValLegend) {
			legendHead = placeTag("thead", null, legendTable, legendBody);

			let head = placeTag("tr", null, legendHead);
			placeTag("th", null, head);

			for (var key in legendCols)
				placeTag("th", LEGEND_LABEL, head).textContent = key;
		}
		else {
			addClass(legendTable, LEGEND_INLINE);
			legend.live && addClass(legendTable, LEGEND_LIVE);
		}
	}

	const son  = {show: true};
	const soff = {show: false};

	function initLegendRow(s, i) {
		if (i == 0 && (multiValLegend || !legend.live || mode == 2))
			return nullNullTuple;

		let cells = [];

		let row = placeTag("tr", LEGEND_SERIES, legendBody, legendBody.childNodes[i]);

		addClass(row, s.class);

		if (!s.show)
			addClass(row, OFF);

		let label = placeTag("th", null, row);

		if (markers.show) {
			let indic = placeDiv(LEGEND_MARKER, label);

			if (i > 0) {
				let width  = markers.width(self, i);

				if (width)
					indic.style.border = width + "px " + markers.dash(self, i) + " " + markers.stroke(self, i);

				indic.style.background = markers.fill(self, i);
			}
		}

		let text = placeDiv(LEGEND_LABEL, label);

		if (s.label instanceof HTMLElement)
			text.appendChild(s.label);
		else
			text.textContent = s.label;

		if (i > 0) {
			if (!markers.show)
				text.style.color = s.width > 0 ? markers.stroke(self, i) : markers.fill(self, i);

			onMouse("click", label, e => {
				if (cursor._lock)
					return;

				setCursorEvent(e);

				let seriesIdx = series.indexOf(s);

				if ((e.ctrlKey || e.metaKey) != legend.isolate) {
					// if any other series is shown, isolate this one. else show all
					let isolate = series.some((s, i) => i > 0 && i != seriesIdx && s.show);

					series.forEach((s, i) => {
						i > 0 && setSeries(i, isolate ? (i == seriesIdx ? son : soff) : son, true, syncOpts.setSeries);
					});
				}
				else
					setSeries(seriesIdx, {show: !s.show}, true, syncOpts.setSeries);
			}, false);

			if (cursorFocus) {
				onMouse(mouseenter, label, e => {
					if (cursor._lock)
						return;

					setCursorEvent(e);

					setSeries(series.indexOf(s), FOCUS_TRUE, true, syncOpts.setSeries);
				}, false);
			}
		}

		for (var key in legendCols) {
			let v = placeTag("td", LEGEND_VALUE, row);
			v.textContent = "--";
			cells.push(v);
		}

		return [row, cells];
	}

	const mouseListeners = new Map();

	function onMouse(ev, targ, fn, onlyTarg = true) {
		const targListeners = mouseListeners.get(targ) || {};
		const listener = cursor.bind[ev](self, targ, fn, onlyTarg);

		if (listener) {
			on(ev, targ, targListeners[ev] = listener);
			mouseListeners.set(targ, targListeners);
		}
	}

	function offMouse(ev, targ, fn) {
		const targListeners = mouseListeners.get(targ) || {};

		for (let k in targListeners) {
			if (ev == null || k == ev) {
				off(k, targ, targListeners[k]);
				delete targListeners[k];
			}
		}

		if (ev == null)
			mouseListeners.delete(targ);
	}

	let fullWidCss = 0;
	let fullHgtCss = 0;

	let plotWidCss = 0;
	let plotHgtCss = 0;

	// plot margins to account for axes
	let plotLftCss = 0;
	let plotTopCss = 0;

	// previous values for diffing
	let _plotLftCss = plotLftCss;
	let _plotTopCss = plotTopCss;
	let _plotWidCss = plotWidCss;
	let _plotHgtCss = plotHgtCss;


	let plotLft = 0;
	let plotTop = 0;
	let plotWid = 0;
	let plotHgt = 0;

	self.bbox = {};

	let shouldSetScales = false;
	let shouldSetSize = false;
	let shouldConvergeSize = false;
	let shouldSetCursor = false;
	let shouldSetSelect = false;
	let shouldSetLegend = false;

	function _setSize(width, height, force) {
		if (force || (width != self.width || height != self.height))
			calcSize(width, height);

		resetYSeries(false);

		shouldConvergeSize = true;
		shouldSetSize = true;

		commit();
	}

	function calcSize(width, height) {
	//	log("calcSize()", arguments);

		self.width  = fullWidCss = plotWidCss = width;
		self.height = fullHgtCss = plotHgtCss = height;
		plotLftCss  = plotTopCss = 0;

		calcPlotRect();
		calcAxesRects();

		let bb = self.bbox;

		plotLft = bb.left   = incrRound(plotLftCss * pxRatio, 0.5);
		plotTop = bb.top    = incrRound(plotTopCss * pxRatio, 0.5);
		plotWid = bb.width  = incrRound(plotWidCss * pxRatio, 0.5);
		plotHgt = bb.height = incrRound(plotHgtCss * pxRatio, 0.5);

	//	updOriDims();
	}

	// ensures size calc convergence
	const CYCLE_LIMIT = 3;

	function convergeSize() {
		let converged = false;

		let cycleNum = 0;

		while (!converged) {
			cycleNum++;

			let axesConverged = axesCalc(cycleNum);
			let paddingConverged = paddingCalc(cycleNum);

			converged = cycleNum == CYCLE_LIMIT || (axesConverged && paddingConverged);

			if (!converged) {
				calcSize(self.width, self.height);
				shouldSetSize = true;
			}
		}
	}

	function setSize({width, height}) {
		_setSize(width, height);
	}

	self.setSize = setSize;

	// accumulate axis offsets, reduce canvas width
	function calcPlotRect() {
		// easements for edge labels
		let hasTopAxis = false;
		let hasBtmAxis = false;
		let hasRgtAxis = false;
		let hasLftAxis = false;

		axes.forEach((axis, i) => {
			if (axis.show && axis._show) {
				let {side, _size} = axis;
				let isVt = side % 2;
				let labelSize = axis.label != null ? axis.labelSize : 0;

				let fullSize = _size + labelSize;

				if (fullSize > 0) {
					if (isVt) {
						plotWidCss -= fullSize;

						if (side == 3) {
							plotLftCss += fullSize;
							hasLftAxis = true;
						}
						else
							hasRgtAxis = true;
					}
					else {
						plotHgtCss -= fullSize;

						if (side == 0) {
							plotTopCss += fullSize;
							hasTopAxis = true;
						}
						else
							hasBtmAxis = true;
					}
				}
			}
		});

		sidesWithAxes[0] = hasTopAxis;
		sidesWithAxes[1] = hasRgtAxis;
		sidesWithAxes[2] = hasBtmAxis;
		sidesWithAxes[3] = hasLftAxis;

		// hz padding
		plotWidCss -= _padding[1] + _padding[3];
		plotLftCss += _padding[3];

		// vt padding
		plotHgtCss -= _padding[2] + _padding[0];
		plotTopCss += _padding[0];
	}

	function calcAxesRects() {
		// will accum +
		let off1 = plotLftCss + plotWidCss;
		let off2 = plotTopCss + plotHgtCss;
		// will accum -
		let off3 = plotLftCss;
		let off0 = plotTopCss;

		function incrOffset(side, size) {
			switch (side) {
				case 1: off1 += size; return off1 - size;
				case 2: off2 += size; return off2 - size;
				case 3: off3 -= size; return off3 + size;
				case 0: off0 -= size; return off0 + size;
			}
		}

		axes.forEach((axis, i) => {
			if (axis.show && axis._show) {
				let side = axis.side;

				axis._pos = incrOffset(side, axis._size);

				if (axis.label != null)
					axis._lpos = incrOffset(side, axis.labelSize);
			}
		});
	}

	if (cursor.dataIdx == null) {
		let hov = cursor.hover;

		let skip = hov.skip = new Set(hov.skip ?? []);
		skip.add(void 0); // alignment artifacts
		let prox = hov.prox = fnOrSelf(hov.prox);
		let bias = hov.bias ??= 0;

		// TODO: only scan between in-view idxs (i0, i1)
		cursor.dataIdx = (self, seriesIdx, cursorIdx, valAtPosX) => {
			if (seriesIdx == 0)
				return cursorIdx;

			let idx2 = cursorIdx;

			let _prox = prox(self, seriesIdx, cursorIdx, valAtPosX) ?? inf;
			let withProx = _prox >= 0 && _prox < inf;
			let xDim = scaleX.ori == 0 ? plotWidCss : plotHgtCss;
			let cursorLft = cursor.left;

			let xValues = data[0];
			let yValues = data[seriesIdx];

			if (skip.has(yValues[cursorIdx])) {
				idx2 = null;

				let nonNullLft = null,
					nonNullRgt = null,
					j;

				if (bias == 0 || bias == -1) {
					j = cursorIdx;
					while (nonNullLft == null && j-- > 0) {
						if (!skip.has(yValues[j]))
							nonNullLft = j;
					}
				}

				if (bias == 0 || bias == 1) {
					j = cursorIdx;
					while (nonNullRgt == null && j++ < yValues.length) {
						if (!skip.has(yValues[j]))
							nonNullRgt = j;
					}
				}

				if (nonNullLft != null || nonNullRgt != null) {
					if (withProx) {
						let lftPos = nonNullLft == null ? -Infinity : valToPosX(xValues[nonNullLft], scaleX, xDim, 0);
						let rgtPos = nonNullRgt == null ?  Infinity : valToPosX(xValues[nonNullRgt], scaleX, xDim, 0);

						let lftDelta = cursorLft - lftPos;
						let rgtDelta = rgtPos - cursorLft;

						if (lftDelta <= rgtDelta) {
							if (lftDelta <= _prox)
								idx2 = nonNullLft;
						} else {
							if (rgtDelta <= _prox)
								idx2 = nonNullRgt;
						}
					}
					else {
						idx2 =
							nonNullRgt == null ? nonNullLft :
							nonNullLft == null ? nonNullRgt :
							cursorIdx - nonNullLft <= nonNullRgt - cursorIdx ? nonNullLft : nonNullRgt;
					}
				}
			}
			else if (withProx) {
				let dist = abs(cursorLft - valToPosX(xValues[cursorIdx], scaleX, xDim, 0));

				if (dist > _prox)
					idx2 = null;
			}

			return idx2;
		};
	}

	const setCursorEvent = e => { cursor.event = e; };

	cursor.idxs = activeIdxs;

	cursor._lock = false;

	let points = cursor.points;

	points.show   = fnOrSelf(points.show);
	points.size   = fnOrSelf(points.size);
	points.stroke = fnOrSelf(points.stroke);
	points.width  = fnOrSelf(points.width);
	points.fill   = fnOrSelf(points.fill);

	const focus = self.focus = assign({}, opts.focus || {alpha: 0.3}, cursor.focus);

	const cursorFocus = focus.prox >= 0;
	const cursorOnePt = cursorFocus && points.one;

	// series-intersection markers
	let cursorPts = [];
	// position caches in CSS pixels
	let cursorPtsLft = [];
	let cursorPtsTop = [];

	function initCursorPt(s, si) {
		let pt = points.show(self, si);

		if (pt instanceof HTMLElement) {
			addClass(pt, CURSOR_PT);
			addClass(pt, s.class);
			elTrans(pt, -10, -10, plotWidCss, plotHgtCss);
			over.insertBefore(pt, cursorPts[si]);

			return pt;
		}
	}

	function initSeries(s, i) {
		if (mode == 1 || i > 0) {
			let isTime = mode == 1 && scales[s.scale].time;

			let sv = s.value;
			s.value = isTime ? (isStr(sv) ? timeSeriesVal(_tzDate, timeSeriesStamp(sv, _fmtDate)) : sv || _timeSeriesVal) : sv || numSeriesVal;
			s.label = s.label || (isTime ? timeSeriesLabel : numSeriesLabel);
		}

		if (cursorOnePt || i > 0) {
			s.width  = s.width == null ? 1 : s.width;
			s.paths  = s.paths || linearPath || retNull;
			s.fillTo = fnOrSelf(s.fillTo || seriesFillTo);
			s.pxAlign = +ifNull(s.pxAlign, pxAlign);
			s.pxRound = pxRoundGen(s.pxAlign);

			s.stroke = fnOrSelf(s.stroke || null);
			s.fill   = fnOrSelf(s.fill || null);
			s._stroke = s._fill = s._paths = s._focus = null;

			let _ptDia = ptDia(max(1, s.width), 1);
			let points = s.points = assign({}, {
				size: _ptDia,
				width: max(1, _ptDia * .2),
				stroke: s.stroke,
				space: _ptDia * 2,
				paths: pointsPath,
				_stroke: null,
				_fill: null,
			}, s.points);
			points.show   = fnOrSelf(points.show);
			points.filter = fnOrSelf(points.filter);
			points.fill   = fnOrSelf(points.fill);
			points.stroke = fnOrSelf(points.stroke);
			points.paths  = fnOrSelf(points.paths);
			points.pxAlign = s.pxAlign;
		}

		if (showLegend) {
			let rowCells = initLegendRow(s, i);
			legendRows.splice(i, 0, rowCells[0]);
			legendCells.splice(i, 0, rowCells[1]);
			legend.values.push(null);	// NULL_LEGEND_VALS not yet avil here :(
		}

		if (showCursor) {
			activeIdxs.splice(i, 0, null);

			let pt = null;

			if (cursorOnePt) {
				if (i == 0)
					pt = initCursorPt(s, i);
			}
			else if (i > 0)
				pt = initCursorPt(s, i);

			cursorPts.splice(i, 0, pt);
			cursorPtsLft.splice(i, 0, 0);
			cursorPtsTop.splice(i, 0, 0);
		}

		fire("addSeries", i);
	}

	function addSeries(opts, si) {
		si = si == null ? series.length : si;

		opts = mode == 1 ? setDefault(opts, si, xSeriesOpts, ySeriesOpts) : setDefault(opts, si, {}, xySeriesOpts);

		series.splice(si, 0, opts);
		initSeries(series[si], si);
	}

	self.addSeries = addSeries;

	function delSeries(i) {
		series.splice(i, 1);

		if (showLegend) {
			legend.values.splice(i, 1);

			legendCells.splice(i, 1);
			let tr = legendRows.splice(i, 1)[0];
			offMouse(null, tr.firstChild);
			tr.remove();
		}

		if (showCursor) {
			activeIdxs.splice(i, 1);
			cursorPts.splice(i, 1)[0].remove();
			cursorPtsLft.splice(i, 1);
			cursorPtsTop.splice(i, 1);
		}

		// TODO: de-init no-longer-needed scales?

		fire("delSeries", i);
	}

	self.delSeries = delSeries;

	const sidesWithAxes = [false, false, false, false];

	function initAxis(axis, i) {
		axis._show = axis.show;

		if (axis.show) {
			let isVt = axis.side % 2;

			let sc = scales[axis.scale];

			// this can occur if all series specify non-default scales
			if (sc == null) {
				axis.scale = isVt ? series[1].scale : xScaleKey;
				sc = scales[axis.scale];
			}

			// also set defaults for incrs & values based on axis distr
			let isTime = sc.time;

			axis.size   = fnOrSelf(axis.size);
			axis.space  = fnOrSelf(axis.space);
			axis.rotate = fnOrSelf(axis.rotate);

			if (isArr(axis.incrs)) {
				axis.incrs.forEach(incr => {
					!fixedDec.has(incr) && fixedDec.set(incr, guessDec(incr));
				});
			}

			axis.incrs  = fnOrSelf(axis.incrs  || (          sc.distr == 2 ? wholeIncrs : (isTime ? (ms == 1 ? timeIncrsMs : timeIncrsS) : numIncrs)));
			axis.splits = fnOrSelf(axis.splits || (isTime && sc.distr == 1 ? _timeAxisSplits : sc.distr == 3 ? logAxisSplits : sc.distr == 4 ? asinhAxisSplits : numAxisSplits));

			axis.stroke        = fnOrSelf(axis.stroke);
			axis.grid.stroke   = fnOrSelf(axis.grid.stroke);
			axis.ticks.stroke  = fnOrSelf(axis.ticks.stroke);
			axis.border.stroke = fnOrSelf(axis.border.stroke);

			let av = axis.values;

			axis.values = (
				// static array of tick values
				isArr(av) && !isArr(av[0]) ? fnOrSelf(av) :
				// temporal
				isTime ? (
					// config array of fmtDate string tpls
					isArr(av) ?
						timeAxisVals(_tzDate, timeAxisStamps(av, _fmtDate)) :
					// fmtDate string tpl
					isStr(av) ?
						timeAxisVal(_tzDate, av) :
					av || _timeAxisVals
				) : av || numAxisVals
			);

			axis.filter = fnOrSelf(axis.filter || (          sc.distr >= 3 && sc.log == 10 ? log10AxisValsFilt : sc.distr == 3 && sc.log == 2 ? log2AxisValsFilt : retArg1));

			axis.font      = pxRatioFont(axis.font);
			axis.labelFont = pxRatioFont(axis.labelFont);

			axis._size   = axis.size(self, null, i, 0);

			axis._space  =
			axis._rotate =
			axis._incrs  =
			axis._found  =	// foundIncrSpace
			axis._splits =
			axis._values = null;

			if (axis._size > 0) {
				sidesWithAxes[i] = true;
				axis._el = placeDiv(AXIS, wrap);
			}

			// debug
		//	axis._el.style.background = "#"  + Math.floor(Math.random()*16777215).toString(16) + '80';
		}
	}

	function autoPadSide(self, side, sidesWithAxes, cycleNum) {
		let [hasTopAxis, hasRgtAxis, hasBtmAxis, hasLftAxis] = sidesWithAxes;

		let ori = side % 2;
		let size = 0;

		if (ori == 0 && (hasLftAxis || hasRgtAxis))
			size = (side == 0 && !hasTopAxis || side == 2 && !hasBtmAxis ? round(xAxisOpts.size / 3) : 0);
		if (ori == 1 && (hasTopAxis || hasBtmAxis))
			size = (side == 1 && !hasRgtAxis || side == 3 && !hasLftAxis ? round(yAxisOpts.size / 2) : 0);

		return size;
	}

	const padding = self.padding = (opts.padding || [autoPadSide,autoPadSide,autoPadSide,autoPadSide]).map(p => fnOrSelf(ifNull(p, autoPadSide)));
	const _padding = self._padding = padding.map((p, i) => p(self, i, sidesWithAxes, 0));

	let dataLen;

	// rendered data window
	let i0 = null;
	let i1 = null;
	const idxs = mode == 1 ? series[0].idxs : null;

	let data0 = null;

	let viaAutoScaleX = false;

	function setData(_data, _resetScales) {
		data = _data == null ? [] : _data;

		self.data = self._data = data;

		if (mode == 2) {
			dataLen = 0;
			for (let i = 1; i < series.length; i++)
				dataLen += data[i][0].length;
		}
		else {
			if (data.length == 0)
				self.data = self._data = data = [[]];

			data0 = data[0];
			dataLen = data0.length;

			let scaleData = data;

			if (xScaleDistr == 2) {
				scaleData = data.slice();

				let _data0 = scaleData[0] = Array(dataLen);
				for (let i = 0; i < dataLen; i++)
					_data0[i] = i;
			}

			self._data = data = scaleData;
		}

		resetYSeries(true);

		fire("setData");

		// forces x axis tick values to re-generate when neither x scale nor y scale changes
		// in ordinal mode, scale range is by index, so will not change if new data has same length, but tick values are from data
		if (xScaleDistr == 2) {
			shouldConvergeSize = true;

			/* or somewhat cheaper, and uglier:
			if (ready) {
				// logic extracted from axesCalc()
				let i = 0;
				let axis = axes[i];
				let _splits = axis._splits.map(i => data0[i]);
				let [_incr, _space] = axis._found;
				let incr = data0[_splits[1]] - data0[_splits[0]];
				axis._values = axis.values(self, axis.filter(self, _splits, i, _space, incr), i, _space, incr);
			}
			*/
		}

		if (_resetScales !== false) {
			let xsc = scaleX;

			if (xsc.auto(self, viaAutoScaleX))
				autoScaleX();
			else
				_setScale(xScaleKey, xsc.min, xsc.max);

			shouldSetCursor = shouldSetCursor || cursor.left >= 0;
			shouldSetLegend = true;
			commit();
		}
	}

	self.setData = setData;

	function autoScaleX() {
		viaAutoScaleX = true;

		let _min, _max;

		if (mode == 1) {
			if (dataLen > 0) {
				i0 = idxs[0] = 0;
				i1 = idxs[1] = dataLen - 1;

				_min = data[0][i0];
				_max = data[0][i1];

				if (xScaleDistr == 2) {
					_min = i0;
					_max = i1;
				}
				else if (_min == _max) {
					if (xScaleDistr == 3)
						[_min, _max] = rangeLog(_min, _min, scaleX.log, false);
					else if (xScaleDistr == 4)
						[_min, _max] = rangeAsinh(_min, _min, scaleX.log, false);
					else if (scaleX.time)
						_max = _min + round(86400 / ms);
					else
						[_min, _max] = rangeNum(_min, _max, rangePad, true);
				}
			}
			else {
				i0 = idxs[0] = _min = null;
				i1 = idxs[1] = _max = null;
			}
		}

		_setScale(xScaleKey, _min, _max);
	}

	let ctxStroke, ctxFill, ctxWidth, ctxDash, ctxJoin, ctxCap, ctxFont, ctxAlign, ctxBaseline;
	let ctxAlpha;

	function setCtxStyle(stroke, width, dash, cap, fill, join) {
		stroke ??= transparent;
		dash   ??= EMPTY_ARR;
		cap    ??= "butt"; // (‿|‿)
		fill   ??= transparent;
		join   ??= "round";

		if (stroke != ctxStroke)
			ctx.strokeStyle = ctxStroke = stroke;
		if (fill != ctxFill)
			ctx.fillStyle = ctxFill = fill;
		if (width != ctxWidth)
			ctx.lineWidth = ctxWidth = width;
		if (join != ctxJoin)
			ctx.lineJoin = ctxJoin = join;
		if (cap != ctxCap)
			ctx.lineCap = ctxCap = cap;
		if (dash != ctxDash)
			ctx.setLineDash(ctxDash = dash);
	}

	function setFontStyle(font, fill, align, baseline) {
		if (fill != ctxFill)
			ctx.fillStyle = ctxFill = fill;
		if (font != ctxFont)
			ctx.font = ctxFont = font;
		if (align != ctxAlign)
			ctx.textAlign = ctxAlign = align;
		if (baseline != ctxBaseline)
			ctx.textBaseline = ctxBaseline = baseline;
	}

	function accScale(wsc, psc, facet, data, sorted = 0) {
		if (data.length > 0 && wsc.auto(self, viaAutoScaleX) && (psc == null || psc.min == null)) {
			let _i0 = ifNull(i0, 0);
			let _i1 = ifNull(i1, data.length - 1);

			// only run getMinMax() for invalidated series data, else reuse
			let minMax = facet.min == null ? getMinMax(data, _i0, _i1, sorted, wsc.distr == 3) : [facet.min, facet.max];

			// initial min/max
			wsc.min = min(wsc.min, facet.min = minMax[0]);
			wsc.max = max(wsc.max, facet.max = minMax[1]);
		}
	}

	const AUTOSCALE = {min: null, max: null};

	function setScales() {
	//	log("setScales()", arguments);

		// implicitly add auto scales, and unranged scales
		for (let k in scales) {
			let sc = scales[k];

			if (pendScales[k] == null &&
				(
					// scales that have never been set (on init)
					sc.min == null ||
					// or auto scales when the x scale was explicitly set
					pendScales[xScaleKey] != null && sc.auto(self, viaAutoScaleX)
				)
			) {
				pendScales[k] = AUTOSCALE;
			}
		}

		// implicitly add dependent scales
		for (let k in scales) {
			let sc = scales[k];

			if (pendScales[k] == null && sc.from != null && pendScales[sc.from] != null)
				pendScales[k] = AUTOSCALE;
		}

		// explicitly setting the x-scale invalidates everything (acts as redraw)
		if (pendScales[xScaleKey] != null)
			resetYSeries(true); // TODO: only reset series on auto scales?

		let wipScales = {};

		for (let k in pendScales) {
			let psc = pendScales[k];

			if (psc != null) {
				let wsc = wipScales[k] = copy(scales[k], fastIsObj);

				if (psc.min != null)
					assign(wsc, psc);
				else if (k != xScaleKey || mode == 2) {
					if (dataLen == 0 && wsc.from == null) {
						let minMax = wsc.range(self, null, null, k);
						wsc.min = minMax[0];
						wsc.max = minMax[1];
					}
					else {
						wsc.min = inf;
						wsc.max = -inf;
					}
				}
			}
		}

		if (dataLen > 0) {
			// pre-range y-scales from y series' data values
			series.forEach((s, i) => {
				if (mode == 1) {
					let k = s.scale;
					let psc = pendScales[k];

					if (psc == null)
						return;

					let wsc = wipScales[k];

					if (i == 0) {
						let minMax = wsc.range(self, wsc.min, wsc.max, k);

						wsc.min = minMax[0];
						wsc.max = minMax[1];

						i0 = closestIdx(wsc.min, data[0]);
						i1 = closestIdx(wsc.max, data[0]);

						// don't try to contract same or adjacent idxs
						if (i1 - i0 > 1) {
							// closest indices can be outside of view
							if (data[0][i0] < wsc.min)
								i0++;
							if (data[0][i1] > wsc.max)
								i1--;
						}

						s.min = data0[i0];
						s.max = data0[i1];
					}
					else if (s.show && s.auto)
						accScale(wsc, psc, s, data[i], s.sorted);

					s.idxs[0] = i0;
					s.idxs[1] = i1;
				}
				else {
					if (i > 0) {
						if (s.show && s.auto) {
							// TODO: only handles, assumes and requires facets[0] / 'x' scale, and facets[1] / 'y' scale
							let [ xFacet, yFacet ] = s.facets;
							let xScaleKey = xFacet.scale;
							let yScaleKey = yFacet.scale;
							let [ xData, yData ] = data[i];

							let wscx = wipScales[xScaleKey];
							let wscy = wipScales[yScaleKey];

							// null can happen when only x is zoomed, but y has static range and doesnt get auto-added to pending
							wscx != null && accScale(wscx, pendScales[xScaleKey], xFacet, xData, xFacet.sorted);
							wscy != null && accScale(wscy, pendScales[yScaleKey], yFacet, yData, yFacet.sorted);

							// temp
							s.min = yFacet.min;
							s.max = yFacet.max;
						}
					}
				}
			});

			// range independent scales
			for (let k in wipScales) {
				let wsc = wipScales[k];
				let psc = pendScales[k];

				if (wsc.from == null && (psc == null || psc.min == null)) {
					let minMax = wsc.range(
						self,
						wsc.min ==  inf ? null : wsc.min,
						wsc.max == -inf ? null : wsc.max,
						k
					);
					wsc.min = minMax[0];
					wsc.max = minMax[1];
				}
			}
		}

		// range dependent scales
		for (let k in wipScales) {
			let wsc = wipScales[k];

			if (wsc.from != null) {
				let base = wipScales[wsc.from];

				if (base.min == null)
					wsc.min = wsc.max = null;
				else {
					let minMax = wsc.range(self, base.min, base.max, k);
					wsc.min = minMax[0];
					wsc.max = minMax[1];
				}
			}
		}

		let changed = {};
		let anyChanged = false;

		for (let k in wipScales) {
			let wsc = wipScales[k];
			let sc = scales[k];

			if (sc.min != wsc.min || sc.max != wsc.max) {
				sc.min = wsc.min;
				sc.max = wsc.max;

				let distr = sc.distr;

				sc._min = distr == 3 ? log10(sc.min) : distr == 4 ? asinh(sc.min, sc.asinh) : distr == 100 ? sc.fwd(sc.min) : sc.min;
				sc._max = distr == 3 ? log10(sc.max) : distr == 4 ? asinh(sc.max, sc.asinh) : distr == 100 ? sc.fwd(sc.max) : sc.max;

				changed[k] = anyChanged = true;
			}
		}

		if (anyChanged) {
			// invalidate paths of all series on changed scales
			series.forEach((s, i) => {
				if (mode == 2) {
					if (i > 0 && changed.y)
						s._paths = null;
				}
				else {
					if (changed[s.scale])
						s._paths = null;
				}
			});

			for (let k in changed) {
				shouldConvergeSize = true;
				fire("setScale", k);
			}

			if (showCursor && cursor.left >= 0)
				shouldSetCursor = shouldSetLegend = true;
		}

		for (let k in pendScales)
			pendScales[k] = null;
	}

	// grabs the nearest indices with y data outside of x-scale limits
	function getOuterIdxs(ydata) {
		let _i0 = clamp(i0 - 1, 0, dataLen - 1);
		let _i1 = clamp(i1 + 1, 0, dataLen - 1);

		while (ydata[_i0] == null && _i0 > 0)
			_i0--;

		while (ydata[_i1] == null && _i1 < dataLen - 1)
			_i1++;

		return [_i0, _i1];
	}

	function drawSeries() {
		if (dataLen > 0) {
			let shouldAlpha = series.some(s => s._focus) && ctxAlpha != focus.alpha;

			if (shouldAlpha)
				ctx.globalAlpha = ctxAlpha = focus.alpha;

			series.forEach((s, i) => {
				if (i > 0 && s.show) {
					cacheStrokeFill(i, false);
					cacheStrokeFill(i, true);

					if (s._paths == null) {
						let _ctxAlpha = ctxAlpha;

						if (ctxAlpha != s.alpha)
							ctx.globalAlpha = ctxAlpha = s.alpha;

						let _idxs = mode == 2 ? [0, data[i][0].length - 1] : getOuterIdxs(data[i]);
						s._paths = s.paths(self, i, _idxs[0], _idxs[1]);

						if (ctxAlpha != _ctxAlpha)
							ctx.globalAlpha = ctxAlpha = _ctxAlpha;
					}
				}
			});

			series.forEach((s, i) => {
				if (i > 0 && s.show) {
					let _ctxAlpha = ctxAlpha;

					if (ctxAlpha != s.alpha)
						ctx.globalAlpha = ctxAlpha = s.alpha;

					s._paths != null && drawPath(i, false);

					{
						let _gaps = s._paths != null ? s._paths.gaps : null;

						let show = s.points.show(self, i, i0, i1, _gaps);
						let idxs = s.points.filter(self, i, show, _gaps);

						if (show || idxs) {
							s.points._paths = s.points.paths(self, i, i0, i1, idxs);
							drawPath(i, true);
						}
					}

					if (ctxAlpha != _ctxAlpha)
						ctx.globalAlpha = ctxAlpha = _ctxAlpha;

					fire("drawSeries", i);
				}
			});

			if (shouldAlpha)
				ctx.globalAlpha = ctxAlpha = 1;
		}
	}

	function cacheStrokeFill(si, _points) {
		let s = _points ? series[si].points : series[si];

		s._stroke = s.stroke(self, si);
		s._fill   = s.fill(self, si);
	}

	function drawPath(si, _points) {
		let s = _points ? series[si].points : series[si];

		let {
			stroke,
			fill,
			clip: gapsClip,
			flags,

			_stroke: strokeStyle = s._stroke,
			_fill:   fillStyle   = s._fill,
			_width:  width       = s.width,
		} = s._paths;

		width = roundDec(width * pxRatio, 3);

		let boundsClip = null;
		let offset = (width % 2) / 2;

		if (_points && fillStyle == null)
			fillStyle = width > 0 ? "#fff" : strokeStyle;

		let _pxAlign = s.pxAlign == 1 && offset > 0;

		_pxAlign && ctx.translate(offset, offset);

		if (!_points) {
			let lft = plotLft - width / 2,
				top = plotTop - width / 2,
				wid = plotWid + width,
				hgt = plotHgt + width;

			boundsClip = new Path2D();
			boundsClip.rect(lft, top, wid, hgt);
		}

		// the points pathbuilder's gapsClip is its boundsClip, since points dont need gaps clipping, and bounds depend on point size
		if (_points)
			strokeFill(strokeStyle, width, s.dash, s.cap, fillStyle, stroke, fill, flags, gapsClip);
		else
			fillStroke(si, strokeStyle, width, s.dash, s.cap, fillStyle, stroke, fill, flags, boundsClip, gapsClip);

		_pxAlign && ctx.translate(-offset, -offset);
	}

	function fillStroke(si, strokeStyle, lineWidth, lineDash, lineCap, fillStyle, strokePath, fillPath, flags, boundsClip, gapsClip) {
		let didStrokeFill = false;

		// for all bands where this series is the top edge, create upwards clips using the bottom edges
		// and apply clips + fill with band fill or dfltFill
		flags != 0 && bands.forEach((b, bi) => {
			// isUpperEdge?
			if (b.series[0] == si) {
				let lowerEdge = series[b.series[1]];
				let lowerData = data[b.series[1]];

				let bandClip = (lowerEdge._paths || EMPTY_OBJ).band;

				if (isArr(bandClip))
					bandClip = b.dir == 1 ? bandClip[0] : bandClip[1];

				let gapsClip2;

				let _fillStyle = null;

				// hasLowerEdge?
				if (lowerEdge.show && bandClip && hasData(lowerData, i0, i1)) {
					_fillStyle = b.fill(self, bi) || fillStyle;
					gapsClip2 = lowerEdge._paths.clip;
				}
				else
					bandClip = null;

				strokeFill(strokeStyle, lineWidth, lineDash, lineCap, _fillStyle, strokePath, fillPath, flags, boundsClip, gapsClip, gapsClip2, bandClip);

				didStrokeFill = true;
			}
		});

		if (!didStrokeFill)
			strokeFill(strokeStyle, lineWidth, lineDash, lineCap, fillStyle, strokePath, fillPath, flags, boundsClip, gapsClip);
	}

	const CLIP_FILL_STROKE = BAND_CLIP_FILL | BAND_CLIP_STROKE;

	function strokeFill(strokeStyle, lineWidth, lineDash, lineCap, fillStyle, strokePath, fillPath, flags, boundsClip, gapsClip, gapsClip2, bandClip) {
		setCtxStyle(strokeStyle, lineWidth, lineDash, lineCap, fillStyle);

		if (boundsClip || gapsClip || bandClip) {
			ctx.save();
			boundsClip && ctx.clip(boundsClip);
			gapsClip && ctx.clip(gapsClip);
		}

		if (bandClip) {
			if ((flags & CLIP_FILL_STROKE) == CLIP_FILL_STROKE) {
				ctx.clip(bandClip);
				gapsClip2 && ctx.clip(gapsClip2);
				doFill(fillStyle, fillPath);
				doStroke(strokeStyle, strokePath, lineWidth);
			}
			else if (flags & BAND_CLIP_STROKE) {
				doFill(fillStyle, fillPath);
				ctx.clip(bandClip);
				doStroke(strokeStyle, strokePath, lineWidth);
			}
			else if (flags & BAND_CLIP_FILL) {
				ctx.save();
				ctx.clip(bandClip);
				gapsClip2 && ctx.clip(gapsClip2);
				doFill(fillStyle, fillPath);
				ctx.restore();
				doStroke(strokeStyle, strokePath, lineWidth);
			}
		}
		else {
			doFill(fillStyle, fillPath);
			doStroke(strokeStyle, strokePath, lineWidth);
		}

		if (boundsClip || gapsClip || bandClip)
			ctx.restore();
	}

	function doStroke(strokeStyle, strokePath, lineWidth) {
		if (lineWidth > 0) {
			if (strokePath instanceof Map) {
				strokePath.forEach((strokePath, strokeStyle) => {
					ctx.strokeStyle = ctxStroke = strokeStyle;
					ctx.stroke(strokePath);
				});
			}
			else
				strokePath != null && strokeStyle && ctx.stroke(strokePath);
		}
	}

	function doFill(fillStyle, fillPath) {
		if (fillPath instanceof Map) {
			fillPath.forEach((fillPath, fillStyle) => {
				ctx.fillStyle = ctxFill = fillStyle;
				ctx.fill(fillPath);
			});
		}
		else
			fillPath != null && fillStyle && ctx.fill(fillPath);
	}

	function getIncrSpace(axisIdx, min, max, fullDim) {
		let axis = axes[axisIdx];

		let incrSpace;

		if (fullDim <= 0)
			incrSpace = [0, 0];
		else {
			let minSpace = axis._space = axis.space(self, axisIdx, min, max, fullDim);
			let incrs    = axis._incrs = axis.incrs(self, axisIdx, min, max, fullDim, minSpace);
			incrSpace    = findIncr(min, max, incrs, fullDim, minSpace);
		}

		return (axis._found = incrSpace);
	}

	function drawOrthoLines(offs, filts, ori, side, pos0, len, width, stroke, dash, cap) {
		let offset = (width % 2) / 2;

		pxAlign == 1 && ctx.translate(offset, offset);

		setCtxStyle(stroke, width, dash, cap, stroke);

		ctx.beginPath();

		let x0, y0, x1, y1, pos1 = pos0 + (side == 0 || side == 3 ? -len : len);

		if (ori == 0) {
			y0 = pos0;
			y1 = pos1;
		}
		else {
			x0 = pos0;
			x1 = pos1;
		}

		for (let i = 0; i < offs.length; i++) {
			if (filts[i] != null) {
				if (ori == 0)
					x0 = x1 = offs[i];
				else
					y0 = y1 = offs[i];

				ctx.moveTo(x0, y0);
				ctx.lineTo(x1, y1);
			}
		}

		ctx.stroke();

		pxAlign == 1 && ctx.translate(-offset, -offset);
	}

	function axesCalc(cycleNum) {
	//	log("axesCalc()", arguments);

		let converged = true;

		axes.forEach((axis, i) => {
			if (!axis.show)
				return;

			let scale = scales[axis.scale];

			if (scale.min == null) {
				if (axis._show) {
					converged = false;
					axis._show = false;
					resetYSeries(false);
				}
				return;
			}
			else {
				if (!axis._show) {
					converged = false;
					axis._show = true;
					resetYSeries(false);
				}
			}

			let side = axis.side;
			let ori = side % 2;

			let {min, max} = scale;		// 		// should this toggle them ._show = false

			let [_incr, _space] = getIncrSpace(i, min, max, ori == 0 ? plotWidCss : plotHgtCss);

			if (_space == 0)
				return;

			// if we're using index positions, force first tick to match passed index
			let forceMin = scale.distr == 2;

			let _splits = axis._splits = axis.splits(self, i, min, max, _incr, _space, forceMin);

			// tick labels
			// BOO this assumes a specific data/series
			let splits = scale.distr == 2 ? _splits.map(i => data0[i]) : _splits;
			let incr   = scale.distr == 2 ? data0[_splits[1]] - data0[_splits[0]] : _incr;

			let values = axis._values = axis.values(self, axis.filter(self, splits, i, _space, incr), i, _space, incr);

			// rotating of labels only supported on bottom x axis
			axis._rotate = side == 2 ? axis.rotate(self, values, i, _space) : 0;

			let oldSize = axis._size;

			axis._size = ceil(axis.size(self, values, i, cycleNum));

			if (oldSize != null && axis._size != oldSize)			// ready && ?
				converged = false;
		});

		return converged;
	}

	function paddingCalc(cycleNum) {
		let converged = true;

		padding.forEach((p, i) => {
			let _p = p(self, i, sidesWithAxes, cycleNum);

			if (_p != _padding[i])
				converged = false;

			_padding[i] = _p;
		});

		return converged;
	}

	function drawAxesGrid() {
		for (let i = 0; i < axes.length; i++) {
			let axis = axes[i];

			if (!axis.show || !axis._show)
				continue;

			let side = axis.side;
			let ori = side % 2;

			let x, y;

			let fillStyle = axis.stroke(self, i);

			let shiftDir = side == 0 || side == 3 ? -1 : 1;

			let [_incr, _space] = axis._found;

			// axis label
			if (axis.label != null) {
				let shiftAmt = axis.labelGap * shiftDir;
				let baseLpos = round((axis._lpos + shiftAmt) * pxRatio);

				setFontStyle(axis.labelFont[0], fillStyle, "center", side == 2 ? TOP : BOTTOM);

				ctx.save();

				if (ori == 1) {
					x = y = 0;

					ctx.translate(
						baseLpos,
						round(plotTop + plotHgt / 2),
					);
					ctx.rotate((side == 3 ? -PI : PI) / 2);

				}
				else {
					x = round(plotLft + plotWid / 2);
					y = baseLpos;
				}

				let _label = isFn(axis.label) ? axis.label(self, i, _incr, _space) : axis.label;

				ctx.fillText(_label, x, y);

				ctx.restore();
			}

			if (_space == 0)
				continue;

			let scale = scales[axis.scale];

			let plotDim = ori == 0 ? plotWid : plotHgt;
			let plotOff = ori == 0 ? plotLft : plotTop;

			let _splits = axis._splits;

			// tick labels
			// BOO this assumes a specific data/series
			let splits = scale.distr == 2 ? _splits.map(i => data0[i]) : _splits;
			let incr   = scale.distr == 2 ? data0[_splits[1]] - data0[_splits[0]] : _incr;

			let ticks = axis.ticks;
			let border = axis.border;
			let _tickSize = ticks.show ? ticks.size : 0;
			let tickSize = round(_tickSize * pxRatio);
			let axisGap = round((axis.alignTo == 2 ? axis._size - _tickSize - axis.gap : axis.gap) * pxRatio);

			// rotating of labels only supported on bottom x axis
			let angle = axis._rotate * -PI/180;

			let basePos  = pxRound(axis._pos * pxRatio);
			let shiftAmt = (tickSize + axisGap) * shiftDir;
			let finalPos = basePos + shiftAmt;
			    y        = ori == 0 ? finalPos : 0;
			    x        = ori == 1 ? finalPos : 0;

			let font         = axis.font[0];
			let textAlign    = axis.align == 1 ? LEFT :
			                   axis.align == 2 ? RIGHT :
			                   angle > 0 ? LEFT :
			                   angle < 0 ? RIGHT :
			                   ori == 0 ? "center" : side == 3 ? RIGHT : LEFT;
			let textBaseline = angle ||
			                   ori == 1 ? "middle" : side == 2 ? TOP   : BOTTOM;

			setFontStyle(font, fillStyle, textAlign, textBaseline);

			let lineHeight = axis.font[1] * axis.lineGap;

			let canOffs = _splits.map(val => pxRound(getPos(val, scale, plotDim, plotOff)));

			let _values = axis._values;

			for (let i = 0; i < _values.length; i++) {
				let val = _values[i];

				if (val != null) {
					if (ori == 0)
						x = canOffs[i];
					else
						y = canOffs[i];

					val = "" + val;

					let _parts = val.indexOf("\n") == -1 ? [val] : val.split(/\n/gm);

					for (let j = 0; j < _parts.length; j++) {
						let text = _parts[j];

						if (angle) {
							ctx.save();
							ctx.translate(x, y + j * lineHeight); // can this be replaced with position math?
							ctx.rotate(angle); // can this be done once?
							ctx.fillText(text, 0, 0);
							ctx.restore();
						}
						else
							ctx.fillText(text, x, y + j * lineHeight);
					}
				}
			}

			// ticks
			if (ticks.show) {
				drawOrthoLines(
					canOffs,
					ticks.filter(self, splits, i, _space, incr),
					ori,
					side,
					basePos,
					tickSize,
					roundDec(ticks.width * pxRatio, 3),
					ticks.stroke(self, i),
					ticks.dash,
					ticks.cap,
				);
			}

			// grid
			let grid = axis.grid;

			if (grid.show) {
				drawOrthoLines(
					canOffs,
					grid.filter(self, splits, i, _space, incr),
					ori,
					ori == 0 ? 2 : 1,
					ori == 0 ? plotTop : plotLft,
					ori == 0 ? plotHgt : plotWid,
					roundDec(grid.width * pxRatio, 3),
					grid.stroke(self, i),
					grid.dash,
					grid.cap,
				);
			}

			if (border.show) {
				drawOrthoLines(
					[basePos],
					[1],
					ori == 0 ? 1 : 0,
					ori == 0 ? 1 : 2,
					ori == 1 ? plotTop : plotLft,
					ori == 1 ? plotHgt : plotWid,
					roundDec(border.width * pxRatio, 3),
					border.stroke(self, i),
					border.dash,
					border.cap,
				);
			}
		}

		fire("drawAxes");
	}

	function resetYSeries(minMax) {
	//	log("resetYSeries()", arguments);

		series.forEach((s, i) => {
			if (i > 0) {
				s._paths = null;

				if (minMax) {
					if (mode == 1) {
						s.min = null;
						s.max = null;
					}
					else {
						s.facets.forEach(f => {
							f.min = null;
							f.max = null;
						});
					}
				}
			}
		});
	}

	let queuedCommit = false;
	let deferHooks = false;
	let hooksQueue = [];

	function flushHooks() {
		deferHooks = false;

		for (let i = 0; i < hooksQueue.length; i++)
			fire(...hooksQueue[i]);

		hooksQueue.length = 0;
	}

	function commit() {
		if (!queuedCommit) {
			microTask(_commit);
			queuedCommit = true;
		}
	}

	// manual batching (aka immediate mode), skips microtask queue
	function batch(fn, _deferHooks = false) {
		queuedCommit = true;
		deferHooks = _deferHooks;

		fn(self);
		_commit();

		if (_deferHooks && hooksQueue.length > 0)
			queueMicrotask(flushHooks);
	}

	self.batch = batch;

	function _commit() {
	//	log("_commit()", arguments);

		if (shouldSetScales) {
			setScales();
			shouldSetScales = false;
		}

		if (shouldConvergeSize) {
			convergeSize();
			shouldConvergeSize = false;
		}

		if (shouldSetSize) {
			setStylePx(under, LEFT,   plotLftCss);
			setStylePx(under, TOP,    plotTopCss);
			setStylePx(under, WIDTH,  plotWidCss);
			setStylePx(under, HEIGHT, plotHgtCss);

			setStylePx(over, LEFT,    plotLftCss);
			setStylePx(over, TOP,     plotTopCss);
			setStylePx(over, WIDTH,   plotWidCss);
			setStylePx(over, HEIGHT,  plotHgtCss);

			setStylePx(wrap, WIDTH,   fullWidCss);
			setStylePx(wrap, HEIGHT,  fullHgtCss);

			// NOTE: mutating this during print preview in Chrome forces transparent
			// canvas pixels to white, even when followed up with clearRect() below
			can.width  = round(fullWidCss * pxRatio);
			can.height = round(fullHgtCss * pxRatio);

			axes.forEach(({ _el, _show, _size, _pos, side }) => {
				if (_el != null) {
					if (_show) {
						let posOffset = (side === 3 || side === 0 ? _size : 0);
						let isVt = side % 2 == 1;

						setStylePx(_el, isVt ? "left"   : "top",    _pos - posOffset);
						setStylePx(_el, isVt ? "width"  : "height", _size);
						setStylePx(_el, isVt ? "top"    : "left",   isVt ? plotTopCss : plotLftCss);
						setStylePx(_el, isVt ? "height" : "width",  isVt ? plotHgtCss : plotWidCss);

						remClass(_el, OFF);
					}
					else
						addClass(_el, OFF);
				}
			});

			// invalidate ctx style cache
			ctxStroke = ctxFill = ctxWidth = ctxJoin = ctxCap = ctxFont = ctxAlign = ctxBaseline = ctxDash = null;
			ctxAlpha = 1;

			syncRect(true);

			if (
				plotLftCss != _plotLftCss ||
				plotTopCss != _plotTopCss ||
				plotWidCss != _plotWidCss ||
				plotHgtCss != _plotHgtCss
			) {
				resetYSeries(false);

				let pctWid = plotWidCss / _plotWidCss;
				let pctHgt = plotHgtCss / _plotHgtCss;

				if (showCursor && !shouldSetCursor && cursor.left >= 0) {
					cursor.left *= pctWid;
					cursor.top  *= pctHgt;

					vCursor && elTrans(vCursor, round(cursor.left), 0, plotWidCss, plotHgtCss);
					hCursor && elTrans(hCursor, 0, round(cursor.top), plotWidCss, plotHgtCss);

					for (let i = 0; i < cursorPts.length; i++) {
						let pt = cursorPts[i];

						if (pt != null) {
							cursorPtsLft[i] *= pctWid;
							cursorPtsTop[i] *= pctHgt;
							elTrans(pt, ceil(cursorPtsLft[i]), ceil(cursorPtsTop[i]), plotWidCss, plotHgtCss);
						}
					}
				}

				if (select.show && !shouldSetSelect && select.left >= 0 && select.width > 0) {
					select.left   *= pctWid;
					select.width  *= pctWid;
					select.top    *= pctHgt;
					select.height *= pctHgt;

					for (let prop in _hideProps)
						setStylePx(selectDiv, prop, select[prop]);
				}

				_plotLftCss = plotLftCss;
				_plotTopCss = plotTopCss;
				_plotWidCss = plotWidCss;
				_plotHgtCss = plotHgtCss;
			}

			fire("setSize");

			shouldSetSize = false;
		}

		if (fullWidCss > 0 && fullHgtCss > 0) {
			ctx.clearRect(0, 0, can.width, can.height);
			fire("drawClear");
			drawOrder.forEach(fn => fn());
			fire("draw");
		}

		if (select.show && shouldSetSelect) {
			setSelect(select);
			shouldSetSelect = false;
		}

		if (showCursor && shouldSetCursor) {
			updateCursor(null, true, false);
			shouldSetCursor = false;
		}

		if (legend.show && legend.live && shouldSetLegend) {
			setLegend();
			shouldSetLegend = false; // redundant currently
		}

		if (!ready) {
			ready = true;
			self.status = 1;

			fire("ready");
		}

		viaAutoScaleX = false;

		queuedCommit = false;
	}

	self.redraw = (rebuildPaths, recalcAxes) => {
		shouldConvergeSize = recalcAxes || false;

		if (rebuildPaths !== false)
			_setScale(xScaleKey, scaleX.min, scaleX.max);
		else
			commit();
	};

	// redraw() => setScale('x', scales.x.min, scales.x.max);

	// explicit, never re-ranged (is this actually true? for x and y)
	function setScale(key, opts) {
		let sc = scales[key];

		if (sc.from == null) {
			if (dataLen == 0) {
				let minMax = sc.range(self, opts.min, opts.max, key);
				opts.min = minMax[0];
				opts.max = minMax[1];
			}

			if (opts.min > opts.max) {
				let _min = opts.min;
				opts.min = opts.max;
				opts.max = _min;
			}

			if (dataLen > 1 && opts.min != null && opts.max != null && opts.max - opts.min < 1e-16)
				return;

			if (key == xScaleKey) {
				if (sc.distr == 2 && dataLen > 0) {
					opts.min = closestIdx(opts.min, data[0]);
					opts.max = closestIdx(opts.max, data[0]);

					if (opts.min == opts.max)
						opts.max++;
				}
			}

		//	log("setScale()", arguments);

			pendScales[key] = opts;

			shouldSetScales = true;
			commit();
		}
	}

	self.setScale = setScale;

//	INTERACTION

	let xCursor;
	let yCursor;
	let vCursor;
	let hCursor;

	// starting position before cursor.move
	let rawMouseLeft0;
	let rawMouseTop0;

	// starting position
	let mouseLeft0;
	let mouseTop0;

	// current position before cursor.move
	let rawMouseLeft1;
	let rawMouseTop1;

	// current position
	let mouseLeft1;
	let mouseTop1;

	let dragging = false;

	const drag = cursor.drag;

	let dragX = drag.x;
	let dragY = drag.y;

	if (showCursor) {
		if (cursor.x)
			xCursor = placeDiv(CURSOR_X, over);
		if (cursor.y)
			yCursor = placeDiv(CURSOR_Y, over);

		if (scaleX.ori == 0) {
			vCursor = xCursor;
			hCursor = yCursor;
		}
		else {
			vCursor = yCursor;
			hCursor = xCursor;
		}

		mouseLeft1 = cursor.left;
		mouseTop1 = cursor.top;
	}

	const select = self.select = assign({
		show:   true,
		over:   true,
		left:   0,
		width:  0,
		top:    0,
		height: 0,
	}, opts.select);

	const selectDiv = select.show ? placeDiv(SELECT, select.over ? over : under) : null;

	function setSelect(opts, _fire) {
		if (select.show) {
			for (let prop in opts) {
				select[prop] = opts[prop];

				if (prop in _hideProps)
					setStylePx(selectDiv, prop, opts[prop]);
			}

			_fire !== false && fire("setSelect");
		}
	}

	self.setSelect = setSelect;

	function toggleDOM(i) {
		let s = series[i];

		if (s.show)
			showLegend && remClass(legendRows[i], OFF);
		else {
			showLegend && addClass(legendRows[i], OFF);

			if (showCursor) {
				let pt = cursorOnePt ? cursorPts[0] : cursorPts[i];
				pt != null && elTrans(pt, -10, -10, plotWidCss, plotHgtCss);
			}
		}
	}

	function _setScale(key, min, max) {
		setScale(key, {min, max});
	}

	function setSeries(i, opts, _fire, _pub) {
	//	log("setSeries()", arguments);

		if (opts.focus != null)
			setFocus(i);

		if (opts.show != null) {
			series.forEach((s, si) => {
				if (si > 0 && (i == si || i == null)) {
					s.show = opts.show;
					toggleDOM(si);

					if (mode == 2) {
						_setScale(s.facets[0].scale, null, null);
						_setScale(s.facets[1].scale, null, null);
					}
					else
						_setScale(s.scale, null, null);

					commit();
				}
			});
		}

		_fire !== false && fire("setSeries", i, opts);

		_pub && pubSync("setSeries", self, i, opts);
	}

	self.setSeries = setSeries;

	function setBand(bi, opts) {
		assign(bands[bi], opts);
	}

	function addBand(opts, bi) {
		opts.fill = fnOrSelf(opts.fill || null);
		opts.dir = ifNull(opts.dir, -1);
		bi = bi == null ? bands.length : bi;
		bands.splice(bi, 0, opts);
	}

	function delBand(bi) {
		if (bi == null)
			bands.length = 0;
		else
			bands.splice(bi, 1);
	}

	self.addBand = addBand;
	self.setBand = setBand;
	self.delBand = delBand;

	function setAlpha(i, value) {
		series[i].alpha = value;

		if (showCursor && cursorPts[i] != null)
			cursorPts[i].style.opacity = value;

		if (showLegend && legendRows[i])
			legendRows[i].style.opacity = value;
	}

	// y-distance
	let closestDist;
	let closestSeries;
	let focusedSeries;
	const FOCUS_TRUE  = {focus: true};

	function setFocus(i) {
		if (i != focusedSeries) {
		//	log("setFocus()", arguments);

			let allFocused = i == null;

			let _setAlpha = focus.alpha != 1;

			series.forEach((s, i2) => {
				if (mode == 1 || i2 > 0) {
					let isFocused = allFocused || i2 == 0 || i2 == i;
					s._focus = allFocused ? null : isFocused;
					_setAlpha && setAlpha(i2, isFocused ? 1 : focus.alpha);
				}
			});

			focusedSeries = i;
			_setAlpha && commit();
		}
	}

	if (showLegend && cursorFocus) {
		onMouse(mouseleave, legendTable, e => {
			if (cursor._lock)
				return;

			setCursorEvent(e);

			if (focusedSeries != null)
				setSeries(null, FOCUS_TRUE, true, syncOpts.setSeries);
		});
	}

	function posToVal(pos, scale, can) {
		let sc = scales[scale];

		if (can)
			pos = pos / pxRatio - (sc.ori == 1 ? plotTopCss : plotLftCss);

		let dim = plotWidCss;

		if (sc.ori == 1) {
			dim = plotHgtCss;
			pos = dim - pos;
		}

		if (sc.dir == -1)
			pos = dim - pos;

		let _min = sc._min,
			_max = sc._max,
			pct = pos / dim;

		let sv = _min + (_max - _min) * pct;

		let distr = sc.distr;

		return (
			distr == 3 ? pow(10, sv) :
			distr == 4 ? sinh(sv, sc.asinh) :
			distr == 100 ? sc.bwd(sv) :
			sv
		);
	}

	function closestIdxFromXpos(pos, can) {
		let v = posToVal(pos, xScaleKey, can);
		return closestIdx(v, data[0], i0, i1);
	}

	self.valToIdx = val => closestIdx(val, data[0]);
	self.posToIdx = closestIdxFromXpos;
	self.posToVal = posToVal;
	self.valToPos = (val, scale, can) => (
		scales[scale].ori == 0 ?
		getHPos(val, scales[scale],
			can ? plotWid : plotWidCss,
			can ? plotLft : 0,
		) :
		getVPos(val, scales[scale],
			can ? plotHgt : plotHgtCss,
			can ? plotTop : 0,
		)
	);

	self.setCursor = (opts, _fire, _pub) => {
		mouseLeft1 = opts.left;
		mouseTop1 = opts.top;
	//	assign(cursor, opts);
		updateCursor(null, _fire, _pub);
	};

	function setSelH(off, dim) {
		setStylePx(selectDiv, LEFT,  select.left = off);
		setStylePx(selectDiv, WIDTH, select.width = dim);
	}

	function setSelV(off, dim) {
		setStylePx(selectDiv, TOP,    select.top = off);
		setStylePx(selectDiv, HEIGHT, select.height = dim);
	}

	let setSelX = scaleX.ori == 0 ? setSelH : setSelV;
	let setSelY = scaleX.ori == 1 ? setSelH : setSelV;

	function syncLegend() {
		if (showLegend && legend.live) {
			for (let i = mode == 2 ? 1 : 0; i < series.length; i++) {
				if (i == 0 && multiValLegend)
					continue;

				let vals = legend.values[i];

				let j = 0;

				for (let k in vals)
					legendCells[i][j++].firstChild.nodeValue = vals[k];
			}
		}
	}

	function setLegend(opts, _fire) {
		if (opts != null) {
			if (opts.idxs) {
				opts.idxs.forEach((didx, sidx) => {
					activeIdxs[sidx] = didx;
				});
			}
			else if (!isUndef(opts.idx))
				activeIdxs.fill(opts.idx);

			legend.idx = activeIdxs[0];
		}

		if (showLegend && legend.live) {
			for (let sidx = 0; sidx < series.length; sidx++) {
				if (sidx > 0 || mode == 1 && !multiValLegend)
					setLegendValues(sidx, activeIdxs[sidx]);
			}

			syncLegend();
		}

		shouldSetLegend = false;

		_fire !== false && fire("setLegend");
	}

	self.setLegend = setLegend;

	function setLegendValues(sidx, idx) {
		let s = series[sidx];
		let src = sidx == 0 && xScaleDistr == 2 ? data0 : data[sidx];
		let val;

		if (multiValLegend)
			val = s.values(self, sidx, idx) ?? NULL_LEGEND_VALUES;
		else {
			val = s.value(self, idx == null ? null : src[idx], sidx, idx);
			val = val == null ? NULL_LEGEND_VALUES : {_: val};
		}

		legend.values[sidx] = val;
	}

	function updateCursor(src, _fire, _pub) {
	//	ts == null && log("updateCursor()", arguments);

		rawMouseLeft1 = mouseLeft1;
		rawMouseTop1 = mouseTop1;

		[mouseLeft1, mouseTop1] = cursor.move(self, mouseLeft1, mouseTop1);

		cursor.left = mouseLeft1;
		cursor.top = mouseTop1;

		if (showCursor) {
			vCursor && elTrans(vCursor, round(mouseLeft1), 0, plotWidCss, plotHgtCss);
			hCursor && elTrans(hCursor, 0, round(mouseTop1), plotWidCss, plotHgtCss);
		}

		let idx;

		// when zooming to an x scale range between datapoints the binary search
		// for nearest min/max indices results in this condition. cheap hack :D
		let noDataInRange = i0 > i1; // works for mode 1 only

		closestDist = inf;
		closestSeries = null;

		// TODO: extract
		let xDim = scaleX.ori == 0 ? plotWidCss : plotHgtCss;
		let yDim = scaleX.ori == 1 ? plotWidCss : plotHgtCss;

		// if cursor hidden, hide points & clear legend vals
		if (mouseLeft1 < 0 || dataLen == 0 || noDataInRange) {
			idx = cursor.idx = null;

			for (let i = 0; i < series.length; i++) {
				let pt = cursorPts[i];
				pt != null && elTrans(pt, -10, -10, plotWidCss, plotHgtCss);
			}

			if (cursorFocus)
				setSeries(null, FOCUS_TRUE, true, src == null && syncOpts.setSeries);

			if (legend.live) {
				activeIdxs.fill(idx);
				shouldSetLegend = true;
			}
		}
		else {
		//	let pctY = 1 - (y / rect.height);

			let mouseXPos, valAtPosX, xPos;

			if (mode == 1) {
				mouseXPos = scaleX.ori == 0 ? mouseLeft1 : mouseTop1;
				valAtPosX = posToVal(mouseXPos, xScaleKey);
				idx = cursor.idx = closestIdx(valAtPosX, data[0], i0, i1);
				xPos = valToPosX(data[0][idx], scaleX, xDim, 0);
			}

			// closest pt values
			let _ptLft = -10;
			let _ptTop = -10;
			let _ptWid = 0;
			let _ptHgt = 0;
			let _centered = true;
			let _ptFill = '';
			let _ptStroke = '';

			for (let i = mode == 2 ? 1 : 0; i < series.length; i++) {
				let s = series[i];

				let idx1  = activeIdxs[i];
				let yVal1 = idx1 == null ? null : (mode == 1 ? data[i][idx1] : data[i][1][idx1]);

				let idx2  = cursor.dataIdx(self, i, idx, valAtPosX);
				let yVal2 = idx2 == null ? null : (mode == 1 ? data[i][idx2] : data[i][1][idx2]);

				shouldSetLegend = shouldSetLegend || yVal2 != yVal1 || idx2 != idx1;

				activeIdxs[i] = idx2;

				if (i > 0 && s.show) {
					let xPos2 = idx2 == null ? -10 : idx2 == idx ? xPos : valToPosX(mode == 1 ? data[0][idx2] : data[i][0][idx2], scaleX, xDim, 0);

					// this doesnt really work for state timeline, heatmap, status history (where the value maps to color, not y coords)
					let yPos = yVal2 == null ? -10 : valToPosY(yVal2, mode == 1 ? scales[s.scale] : scales[s.facets[1].scale], yDim, 0);

					if (cursorFocus && yVal2 != null) {
						let mouseYPos = scaleX.ori == 1 ? mouseLeft1 : mouseTop1;
						let dist = abs(focus.dist(self, i, idx2, yPos, mouseYPos));

						if (dist < closestDist) {
							let bias = focus.bias;

							if (bias != 0) {
								let mouseYVal = posToVal(mouseYPos, s.scale);

								let seriesYValSign = yVal2     >= 0 ? 1 : -1;
								let mouseYValSign  = mouseYVal >= 0 ? 1 : -1;

								// with a focus bias, we will never cross zero when prox testing
								// it's either closest towards zero, or closest away from zero
								if (mouseYValSign == seriesYValSign && (
									mouseYValSign == 1 ?
										(bias == 1 ? yVal2 >= mouseYVal : yVal2 <= mouseYVal) :  // >= 0
										(bias == 1 ? yVal2 <= mouseYVal : yVal2 >= mouseYVal)    //  < 0
								)) {
									closestDist = dist;
									closestSeries = i;
								}
							}
							else {
								closestDist = dist;
								closestSeries = i;
							}
						}
					}

					if (shouldSetLegend || cursorOnePt) {
						let hPos, vPos;

						if (scaleX.ori == 0) {
							hPos = xPos2;
							vPos = yPos;
						}
						else {
							hPos = yPos;
							vPos = xPos2;
						}

						let ptWid, ptHgt, ptLft, ptTop,
							ptStroke, ptFill,
							centered = true,
							getBBox = points.bbox;

						if (getBBox != null) {
							centered = false;

							let bbox = getBBox(self, i);

							ptLft = bbox.left;
							ptTop = bbox.top;
							ptWid = bbox.width;
							ptHgt = bbox.height;
						}
						else {
							ptLft = hPos;
							ptTop = vPos;
							ptWid = ptHgt = points.size(self, i);
						}

						ptFill = points.fill(self, i);
						ptStroke = points.stroke(self, i);

						if (cursorOnePt) {
							if (i == closestSeries && closestDist <= focus.prox) {
								_ptLft = ptLft;
								_ptTop = ptTop;
								_ptWid = ptWid;
								_ptHgt = ptHgt;
								_centered = centered;
								_ptFill = ptFill;
								_ptStroke = ptStroke;
							}
						}
						else {
							let pt = cursorPts[i];

							if (pt != null) {
								cursorPtsLft[i] = ptLft;
								cursorPtsTop[i] = ptTop;

								elSize(pt, ptWid, ptHgt, centered);
								elColor(pt, ptFill, ptStroke);
								elTrans(pt, ceil(ptLft), ceil(ptTop), plotWidCss, plotHgtCss);
							}
						}
					}
				}
			}

			// if only using single hover point (at cursorPts[0])
			// we have trigger styling at last visible series (once closestSeries is settled)
			if (cursorOnePt) {
				// some of this logic is similar to series focus below, since it matches the behavior by design

				let p = focus.prox;

				let focusChanged = focusedSeries == null ? closestDist <= p : (closestDist > p || closestSeries != focusedSeries);

				if (shouldSetLegend || focusChanged) {
					let pt = cursorPts[0];

					if (pt != null) {
						cursorPtsLft[0] = _ptLft;
						cursorPtsTop[0] = _ptTop;

						elSize(pt, _ptWid, _ptHgt, _centered);
						elColor(pt, _ptFill, _ptStroke);
						elTrans(pt, ceil(_ptLft), ceil(_ptTop), plotWidCss, plotHgtCss);
					}
				}
			}
		}

		// nit: cursor.drag.setSelect is assumed always true
		if (select.show && dragging) {
			if (src != null) {
				let [xKey, yKey] = syncOpts.scales;
				let [matchXKeys, matchYKeys] = syncOpts.match;
				let [xKeySrc, yKeySrc] = src.cursor.sync.scales;

				// match the dragX/dragY implicitness/explicitness of src
				let sdrag = src.cursor.drag;
				dragX = sdrag._x;
				dragY = sdrag._y;

				if (dragX || dragY) {
					let { left, top, width, height } = src.select;

					let sori = src.scales[xKeySrc].ori;
					let sPosToVal = src.posToVal;

					let sOff, sDim, sc, a, b;

					let matchingX = xKey != null && matchXKeys(xKey, xKeySrc);
					let matchingY = yKey != null && matchYKeys(yKey, yKeySrc);

					if (matchingX && dragX) {
						if (sori == 0) {
							sOff = left;
							sDim = width;
						}
						else {
							sOff = top;
							sDim = height;
						}

						sc = scales[xKey];

						a = valToPosX(sPosToVal(sOff, xKeySrc),        sc, xDim, 0);
						b = valToPosX(sPosToVal(sOff + sDim, xKeySrc), sc, xDim, 0);

						setSelX(min(a,b), abs(b-a));
					}
					else
						setSelX(0, xDim);

					if (matchingY && dragY) {
						if (sori == 1) {
							sOff = left;
							sDim = width;
						}
						else {
							sOff = top;
							sDim = height;
						}

						sc = scales[yKey];

						a = valToPosY(sPosToVal(sOff, yKeySrc),        sc, yDim, 0);
						b = valToPosY(sPosToVal(sOff + sDim, yKeySrc), sc, yDim, 0);

						setSelY(min(a,b), abs(b-a));
					}
					else
						setSelY(0, yDim);
				}
				else
					hideSelect();
			}
			else {
				let rawDX = abs(rawMouseLeft1 - rawMouseLeft0);
				let rawDY = abs(rawMouseTop1 - rawMouseTop0);

				if (scaleX.ori == 1) {
					let _rawDX = rawDX;
					rawDX = rawDY;
					rawDY = _rawDX;
				}

				dragX = drag.x && rawDX >= drag.dist;
				dragY = drag.y && rawDY >= drag.dist;

				let uni = drag.uni;

				if (uni != null) {
					// only calc drag status if they pass the dist thresh
					if (dragX && dragY) {
						dragX = rawDX >= uni;
						dragY = rawDY >= uni;

						// force unidirectionality when both are under uni limit
						if (!dragX && !dragY) {
							if (rawDY > rawDX)
								dragY = true;
							else
								dragX = true;
						}
					}
				}
				else if (drag.x && drag.y && (dragX || dragY))
					// if omni with no uni then both dragX / dragY should be true if either is true
					dragX = dragY = true;

				let p0, p1;

				if (dragX) {
					if (scaleX.ori == 0) {
						p0 = mouseLeft0;
						p1 = mouseLeft1;
					}
					else {
						p0 = mouseTop0;
						p1 = mouseTop1;
					}

					setSelX(min(p0, p1), abs(p1 - p0));

					if (!dragY)
						setSelY(0, yDim);
				}

				if (dragY) {
					if (scaleX.ori == 1) {
						p0 = mouseLeft0;
						p1 = mouseLeft1;
					}
					else {
						p0 = mouseTop0;
						p1 = mouseTop1;
					}

					setSelY(min(p0, p1), abs(p1 - p0));

					if (!dragX)
						setSelX(0, xDim);
				}

				// the drag didn't pass the dist requirement
				if (!dragX && !dragY) {
					setSelX(0, 0);
					setSelY(0, 0);
				}
			}
		}

		drag._x = dragX;
		drag._y = dragY;

		if (src == null) {
			if (_pub) {
				if (syncKey != null) {
					let [xSyncKey, ySyncKey] = syncOpts.scales;

					syncOpts.values[0] = xSyncKey != null ? posToVal(scaleX.ori == 0 ? mouseLeft1 : mouseTop1, xSyncKey) : null;
					syncOpts.values[1] = ySyncKey != null ? posToVal(scaleX.ori == 1 ? mouseLeft1 : mouseTop1, ySyncKey) : null;
				}

				pubSync(mousemove, self, mouseLeft1, mouseTop1, plotWidCss, plotHgtCss, idx);
			}

			if (cursorFocus) {
				let shouldPub = _pub && syncOpts.setSeries;
				let p = focus.prox;

				if (focusedSeries == null) {
					if (closestDist <= p)
						setSeries(closestSeries, FOCUS_TRUE, true, shouldPub);
				}
				else {
					if (closestDist > p)
						setSeries(null, FOCUS_TRUE, true, shouldPub);
					else if (closestSeries != focusedSeries)
						setSeries(closestSeries, FOCUS_TRUE, true, shouldPub);
				}
			}
		}

		if (shouldSetLegend) {
			legend.idx = idx;
			setLegend();
		}

		_fire !== false && fire("setCursor");
	}

	let rect = null;

	Object.defineProperty(self, 'rect', {
		get() {
			if (rect == null)
				syncRect(false);

			return rect;
		},
	});

	function syncRect(defer = false) {
		if (defer)
			rect = null;
		else {
			rect = over.getBoundingClientRect();
			fire("syncRect", rect);
		}
	}

	function mouseMove(e, src, _l, _t, _w, _h, _i) {
		if (cursor._lock)
			return;

		// Chrome on Windows has a bug which triggers a stray mousemove event after an initial mousedown event
		// when clicking into a plot as part of re-focusing the browser window.
		// we gotta ignore it to avoid triggering a phantom drag / setSelect
		// However, on touch-only devices Chrome-based browsers trigger a 0-distance mousemove before mousedown
		// so we don't ignore it when mousedown has set the dragging flag
		if (dragging && e != null && e.movementX == 0 && e.movementY == 0)
			return;

		cacheMouse(e, src, _l, _t, _w, _h, _i, false, e != null);

		if (e != null)
			updateCursor(null, true, true);
		else
			updateCursor(src, true, false);
	}

	function cacheMouse(e, src, _l, _t, _w, _h, _i, initial, snap) {
		if (rect == null)
			syncRect(false);

		setCursorEvent(e);

		if (e != null) {
			_l = e.clientX - rect.left;
			_t = e.clientY - rect.top;
		}
		else {
			if (_l < 0 || _t < 0) {
				mouseLeft1 = -10;
				mouseTop1 = -10;
				return;
			}

			let [xKey, yKey] = syncOpts.scales;

			let syncOptsSrc = src.cursor.sync;
			let [xValSrc, yValSrc] = syncOptsSrc.values;
			let [xKeySrc, yKeySrc] = syncOptsSrc.scales;
			let [matchXKeys, matchYKeys] = syncOpts.match;

			let rotSrc = src.axes[0].side % 2 == 1;

			let xDim = scaleX.ori == 0 ? plotWidCss : plotHgtCss,
				yDim = scaleX.ori == 1 ? plotWidCss : plotHgtCss,
				_xDim = rotSrc ? _h : _w,
				_yDim = rotSrc ? _w : _h,
				_xPos = rotSrc ? _t : _l,
				_yPos = rotSrc ? _l : _t;

			if (xKeySrc != null)
				_l = matchXKeys(xKey, xKeySrc) ? getPos(xValSrc, scales[xKey], xDim, 0) : -10;
			else
				_l = xDim * (_xPos/_xDim);

			if (yKeySrc != null)
				_t = matchYKeys(yKey, yKeySrc) ? getPos(yValSrc, scales[yKey], yDim, 0) : -10;
			else
				_t = yDim * (_yPos/_yDim);

			if (scaleX.ori == 1) {
				let __l = _l;
				_l = _t;
				_t = __l;
			}
		}

		if (snap && (src == null || src.cursor.event.type == mousemove)) {
			if (_l <= 1 || _l >= plotWidCss - 1)
				_l = incrRound(_l, plotWidCss);

			if (_t <= 1 || _t >= plotHgtCss - 1)
				_t = incrRound(_t, plotHgtCss);
		}

		if (initial) {
			rawMouseLeft0 = _l;
			rawMouseTop0 = _t;

			[mouseLeft0, mouseTop0] = cursor.move(self, _l, _t);
		}
		else {
			mouseLeft1 = _l;
			mouseTop1 = _t;
		}
	}

	const _hideProps = {
		width: 0,
		height: 0,
		left: 0,
		top: 0,
	};

	function hideSelect() {
		setSelect(_hideProps, false);
	}

	let downSelectLeft;
	let downSelectTop;
	let downSelectWidth;
	let downSelectHeight;

	function mouseDown(e, src, _l, _t, _w, _h, _i) {
		dragging = true;
		dragX = dragY = drag._x = drag._y = false;

		cacheMouse(e, src, _l, _t, _w, _h, _i, true, false);

		if (e != null) {
			onMouse(mouseup, doc, mouseUp, false);
			pubSync(mousedown, self, mouseLeft0, mouseTop0, plotWidCss, plotHgtCss, null);
		}

		let { left, top, width, height } = select;

		downSelectLeft   = left;
		downSelectTop    = top;
		downSelectWidth  = width;
		downSelectHeight = height;

	//	hideSelect();
	}

	function mouseUp(e, src, _l, _t, _w, _h, _i) {
		dragging = drag._x = drag._y = false;

		cacheMouse(e, src, _l, _t, _w, _h, _i, false, true);

		let { left, top, width, height } = select;

		let hasSelect = width > 0 || height > 0;
		let chgSelect = (
			downSelectLeft   != left   ||
			downSelectTop    != top    ||
			downSelectWidth  != width  ||
			downSelectHeight != height
		);

		hasSelect && chgSelect && setSelect(select);

		if (drag.setScale && hasSelect && chgSelect) {
		//	if (syncKey != null) {
		//		dragX = drag.x;
		//		dragY = drag.y;
		//	}

			let xOff = left,
				xDim = width,
				yOff = top,
				yDim = height;

			if (scaleX.ori == 1) {
				xOff = top,
				xDim = height,
				yOff = left,
				yDim = width;
			}

			if (dragX) {
				_setScale(xScaleKey,
					posToVal(xOff, xScaleKey),
					posToVal(xOff + xDim, xScaleKey)
				);
			}

			if (dragY) {
				for (let k in scales) {
					let sc = scales[k];

					if (k != xScaleKey && sc.from == null && sc.min != inf) {
						_setScale(k,
							posToVal(yOff + yDim, k),
							posToVal(yOff, k)
						);
					}
				}
			}

			hideSelect();
		}
		else if (cursor.lock) {
			cursor._lock = !cursor._lock;
			updateCursor(src, true, e != null);
		}

		if (e != null) {
			offMouse(mouseup, doc);
			pubSync(mouseup, self, mouseLeft1, mouseTop1, plotWidCss, plotHgtCss, null);
		}
	}

	function mouseLeave(e, src, _l, _t, _w, _h, _i) {
		if (cursor._lock)
			return;

		setCursorEvent(e);

		let _dragging = dragging;

		if (dragging) {
			// handle case when mousemove aren't fired all the way to edges by browser
			let snapH = true;
			let snapV = true;
			let snapProx = 10;

			let dragH, dragV;

			if (scaleX.ori == 0) {
				dragH = dragX;
				dragV = dragY;
			}
			else {
				dragH = dragY;
				dragV = dragX;
			}

			if (dragH && dragV) {
				// maybe omni corner snap
				snapH = mouseLeft1 <= snapProx || mouseLeft1 >= plotWidCss - snapProx;
				snapV = mouseTop1  <= snapProx || mouseTop1  >= plotHgtCss - snapProx;
			}

			if (dragH && snapH)
				mouseLeft1 = mouseLeft1 < mouseLeft0 ? 0 : plotWidCss;

			if (dragV && snapV)
				mouseTop1 = mouseTop1 < mouseTop0 ? 0 : plotHgtCss;

			updateCursor(null, true, true);

			dragging = false;
		}

		mouseLeft1 = -10;
		mouseTop1 = -10;

		activeIdxs.fill(null);

		// passing a non-null timestamp to force sync/mousemove event
		updateCursor(null, true, true);

		if (_dragging)
			dragging = _dragging;
	}

	function dblClick(e, src, _l, _t, _w, _h, _i) {
		if (cursor._lock)
			return;

		setCursorEvent(e);

		autoScaleX();

		hideSelect();

		if (e != null)
			pubSync(dblclick, self, mouseLeft1, mouseTop1, plotWidCss, plotHgtCss, null);
	}

	function syncPxRatio() {
		axes.forEach(syncFontSize);
		_setSize(self.width, self.height, true);
	}

	on(dppxchange, win, syncPxRatio);

	// internal pub/sub
	const events = {};

	events.mousedown = mouseDown;
	events.mousemove = mouseMove;
	events.mouseup = mouseUp;
	events.dblclick = dblClick;
	events["setSeries"] = (e, src, idx, opts) => {
		let seriesIdxMatcher = syncOpts.match[2];
		idx = seriesIdxMatcher(self, src, idx);
		idx != -1 && setSeries(idx, opts, true, false);
	};

	if (showCursor) {
		onMouse(mousedown,  over, mouseDown);
		onMouse(mousemove,  over, mouseMove);
		onMouse(mouseenter, over, e => {
			setCursorEvent(e);
			syncRect(false);
		});
		onMouse(mouseleave, over, mouseLeave);

		onMouse(dblclick, over, dblClick);

		cursorPlots.add(self);

		self.syncRect = syncRect;
	}

	// external on/off
	const hooks = self.hooks = opts.hooks || {};

	function fire(evName, a1, a2) {
		if (deferHooks)
			hooksQueue.push([evName, a1, a2]);
		else {
			if (evName in hooks) {
				hooks[evName].forEach(fn => {
					fn.call(null, self, a1, a2);
				});
			}
		}
	}

	(opts.plugins || []).forEach(p => {
		for (let evName in p.hooks)
			hooks[evName] = (hooks[evName] || []).concat(p.hooks[evName]);
	});

	const seriesIdxMatcher = (self, src, srcSeriesIdx) => srcSeriesIdx;

	const syncOpts = assign({
		key: null,
		setSeries: false,
		filters: {
			pub: retTrue,
			sub: retTrue,
		},
		scales: [xScaleKey, series[1] ? series[1].scale : null],
		match: [retEq, retEq, seriesIdxMatcher],
		values: [null, null],
	}, cursor.sync);

	if (syncOpts.match.length == 2)
		syncOpts.match.push(seriesIdxMatcher);

	cursor.sync = syncOpts;

	const syncKey = syncOpts.key;

	const sync = _sync(syncKey);

	function pubSync(type, src, x, y, w, h, i) {
		if (syncOpts.filters.pub(type, src, x, y, w, h, i))
			sync.pub(type, src, x, y, w, h, i);
	}

	sync.sub(self);

	function pub(type, src, x, y, w, h, i) {
		if (syncOpts.filters.sub(type, src, x, y, w, h, i))
			events[type](null, src, x, y, w, h, i);
	}

	self.pub = pub;

	function destroy() {
		sync.unsub(self);
		cursorPlots.delete(self);
		mouseListeners.clear();
		off(dppxchange, win, syncPxRatio);
		root.remove();
		legendTable?.remove(); // in case mounted outside of root
		fire("destroy");
	}

	self.destroy = destroy;

	function _init() {
		fire("init", opts, data);

		setData(data || opts.data, false);

		if (pendScales[xScaleKey])
			setScale(xScaleKey, pendScales[xScaleKey]);
		else
			autoScaleX();

		shouldSetSelect = select.show && (select.width > 0 || select.height > 0);
		shouldSetCursor = shouldSetLegend = true;

		_setSize(opts.width, opts.height);
	}

	series.forEach(initSeries);

	axes.forEach(initAxis);

	if (then) {
		if (then instanceof HTMLElement) {
			then.appendChild(root);
			_init();
		}
		else
			then(self, _init);
	}
	else
		_init();

	return self;
}

uPlot.assign = assign;
uPlot.fmtNum = fmtNum;
uPlot.rangeNum = rangeNum;
uPlot.rangeLog = rangeLog;
uPlot.rangeAsinh = rangeAsinh;
uPlot.orient   = orient;
uPlot.pxRatio = pxRatio;

{
	uPlot.join = join;
}

{
	uPlot.fmtDate = fmtDate;
	uPlot.tzDate  = tzDate;
}

uPlot.sync = _sync;

{
	uPlot.addGap = addGap;
	uPlot.clipGaps = clipGaps;

	let paths = uPlot.paths = {
		points,
	};

	(paths.linear  = linear);
	(paths.stepped = stepped);
	(paths.bars    = bars);
	(paths.spline  = monotoneCubic);
}




}),
"./src/lib/components/meteo-dashboard.ts": 
/*!***********************************************!*\
  !*** ./src/lib/components/meteo-dashboard.ts ***!
  \***********************************************/
(function (__unused_webpack_module, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  MeteoDashboard: () => (MeteoDashboard)
});
/* ESM import */var lit__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! lit */ "./node_modules/.pnpm/lit@3.3.1/node_modules/lit/index.js");
/* ESM import */var uplot__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! uplot */ "./node_modules/.pnpm/uplot@1.6.32/node_modules/uplot/dist/uPlot.esm.js");
/* ESM import */var js_yaml__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! js-yaml */ "./node_modules/.pnpm/js-yaml@4.1.0/node_modules/js-yaml/dist/js-yaml.mjs");
function _array_like_to_array(arr, len) {
    if (len == null || len > arr.length) len = arr.length;
    for(var i = 0, arr2 = new Array(len); i < len; i++)arr2[i] = arr[i];
    return arr2;
}
function _array_with_holes(arr) {
    if (Array.isArray(arr)) return arr;
}
function _assert_this_initialized(self) {
    if (self === void 0) {
        throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self;
}
function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) {
    try {
        var info = gen[key](arg);
        var value = info.value;
    } catch (error) {
        reject(error);
        return;
    }
    if (info.done) {
        resolve(value);
    } else {
        Promise.resolve(value).then(_next, _throw);
    }
}
function _async_to_generator(fn) {
    return function() {
        var self = this, args = arguments;
        return new Promise(function(resolve, reject) {
            var gen = fn.apply(self, args);
            function _next(value) {
                asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value);
            }
            function _throw(err) {
                asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err);
            }
            _next(undefined);
        });
    };
}
function _call_super(_this, derived, args) {
    derived = _get_prototype_of(derived);
    return _possible_constructor_return(_this, _is_native_reflect_construct() ? Reflect.construct(derived, args || [], _get_prototype_of(_this).constructor) : derived.apply(_this, args));
}
function _class_call_check(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
        throw new TypeError("Cannot call a class as a function");
    }
}
function _defineProperties(target, props) {
    for(var i = 0; i < props.length; i++){
        var descriptor = props[i];
        descriptor.enumerable = descriptor.enumerable || false;
        descriptor.configurable = true;
        if ("value" in descriptor) descriptor.writable = true;
        Object.defineProperty(target, descriptor.key, descriptor);
    }
}
function _create_class(Constructor, protoProps, staticProps) {
    if (protoProps) _defineProperties(Constructor.prototype, protoProps);
    if (staticProps) _defineProperties(Constructor, staticProps);
    return Constructor;
}
function _define_property(obj, key, value) {
    if (key in obj) {
        Object.defineProperty(obj, key, {
            value: value,
            enumerable: true,
            configurable: true,
            writable: true
        });
    } else {
        obj[key] = value;
    }
    return obj;
}
function _get(target, property, receiver) {
    if (typeof Reflect !== "undefined" && Reflect.get) {
        _get = Reflect.get;
    } else {
        _get = function get(target, property, receiver) {
            var base = _super_prop_base(target, property);
            if (!base) return;
            var desc = Object.getOwnPropertyDescriptor(base, property);
            if (desc.get) {
                return desc.get.call(receiver || target);
            }
            return desc.value;
        };
    }
    return _get(target, property, receiver || target);
}
function _get_prototype_of(o) {
    _get_prototype_of = Object.setPrototypeOf ? Object.getPrototypeOf : function getPrototypeOf(o) {
        return o.__proto__ || Object.getPrototypeOf(o);
    };
    return _get_prototype_of(o);
}
function _inherits(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
        throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, {
        constructor: {
            value: subClass,
            writable: true,
            configurable: true
        }
    });
    if (superClass) _set_prototype_of(subClass, superClass);
}
function _iterable_to_array_limit(arr, i) {
    var _i = arr == null ? null : typeof Symbol !== "undefined" && arr[Symbol.iterator] || arr["@@iterator"];
    if (_i == null) return;
    var _arr = [];
    var _n = true;
    var _d = false;
    var _s, _e;
    try {
        for(_i = _i.call(arr); !(_n = (_s = _i.next()).done); _n = true){
            _arr.push(_s.value);
            if (i && _arr.length === i) break;
        }
    } catch (err) {
        _d = true;
        _e = err;
    } finally{
        try {
            if (!_n && _i["return"] != null) _i["return"]();
        } finally{
            if (_d) throw _e;
        }
    }
    return _arr;
}
function _non_iterable_rest() {
    throw new TypeError("Invalid attempt to destructure non-iterable instance.\\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}
function _possible_constructor_return(self, call) {
    if (call && (_type_of(call) === "object" || typeof call === "function")) {
        return call;
    }
    return _assert_this_initialized(self);
}
function _set_prototype_of(o, p) {
    _set_prototype_of = Object.setPrototypeOf || function setPrototypeOf(o, p) {
        o.__proto__ = p;
        return o;
    };
    return _set_prototype_of(o, p);
}
function _sliced_to_array(arr, i) {
    return _array_with_holes(arr) || _iterable_to_array_limit(arr, i) || _unsupported_iterable_to_array(arr, i) || _non_iterable_rest();
}
function _super_prop_base(object, property) {
    while(!Object.prototype.hasOwnProperty.call(object, property)){
        object = _get_prototype_of(object);
        if (object === null) break;
    }
    return object;
}
function _tagged_template_literal(strings, raw) {
    if (!raw) {
        raw = strings.slice(0);
    }
    return Object.freeze(Object.defineProperties(strings, {
        raw: {
            value: Object.freeze(raw)
        }
    }));
}
function _type_of(obj) {
    "@swc/helpers - typeof";
    return obj && typeof Symbol !== "undefined" && obj.constructor === Symbol ? "symbol" : typeof obj;
}
function _unsupported_iterable_to_array(o, minLen) {
    if (!o) return;
    if (typeof o === "string") return _array_like_to_array(o, minLen);
    var n = Object.prototype.toString.call(o).slice(8, -1);
    if (n === "Object" && o.constructor) n = o.constructor.name;
    if (n === "Map" || n === "Set") return Array.from(n);
    if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _array_like_to_array(o, minLen);
}
function _is_native_reflect_construct() {
    try {
        var result = !Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {}));
    } catch (_) {}
    return (_is_native_reflect_construct = function() {
        return !!result;
    })();
}
function _ts_generator(thisArg, body) {
    var f, y, t, _ = {
        label: 0,
        sent: function() {
            if (t[0] & 1) throw t[1];
            return t[1];
        },
        trys: [],
        ops: []
    }, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() {
        return this;
    }), g;
    function verb(n) {
        return function(v) {
            return step([
                n,
                v
            ]);
        };
    }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while(g && (g = 0, op[0] && (_ = 0)), _)try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [
                op[0] & 2,
                t.value
            ];
            switch(op[0]){
                case 0:
                case 1:
                    t = op;
                    break;
                case 4:
                    _.label++;
                    return {
                        value: op[1],
                        done: false
                    };
                case 5:
                    _.label++;
                    y = op[1];
                    op = [
                        0
                    ];
                    continue;
                case 7:
                    op = _.ops.pop();
                    _.trys.pop();
                    continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) {
                        _ = 0;
                        continue;
                    }
                    if (op[0] === 3 && (!t || op[1] > t[0] && op[1] < t[3])) {
                        _.label = op[1];
                        break;
                    }
                    if (op[0] === 6 && _.label < t[1]) {
                        _.label = t[1];
                        t = op;
                        break;
                    }
                    if (t && _.label < t[2]) {
                        _.label = t[2];
                        _.ops.push(op);
                        break;
                    }
                    if (t[2]) _.ops.pop();
                    _.trys.pop();
                    continue;
            }
            op = body.call(thisArg, _);
        } catch (e) {
            op = [
                6,
                e
            ];
            y = 0;
        } finally{
            f = t = 0;
        }
        if (op[0] & 5) throw op[1];
        return {
            value: op[0] ? op[1] : void 0,
            done: true
        };
    }
}
function _templateObject() {
    var data = _tagged_template_literal([
        ""
    ]);
    _templateObject = function _templateObject() {
        return data;
    };
    return data;
}



var MeteoDashboard = /*#__PURE__*/ function(LitElement) {
    "use strict";
    _inherits(MeteoDashboard, LitElement);
    function MeteoDashboard() {
        _class_call_check(this, MeteoDashboard);
        var _this;
        _this = _call_super(this, MeteoDashboard, arguments), _define_property(_this, "statusEl", void 0), _define_property(_this, "lastTempEl", void 0), _define_property(_this, "lastEnergyEl", void 0), _define_property(_this, "chartEl", void 0), _define_property(_this, "progressEl", void 0), _define_property(_this, "currentTimeEl", void 0), _define_property(_this, "nextInEl", void 0), _define_property(_this, "btnPlay", null), _define_property(_this, "toggleTempBtn", void 0), _define_property(_this, "toggleEnergyBtn", void 0), _define_property(_this, "timer", null), _define_property(_this, "secondTimer", null), _define_property(_this, "nextAt", 0), _define_property(_this, "playing", true), _define_property(_this, "_onThemeChanged", void 0), _define_property(_this, "data", null), _define_property(_this, "pointer", 0), _define_property(_this, "perMinute", new Map()), _define_property(_this, "plot", null), _define_property(_this, "resizeObs", null), _define_property(_this, "themeObs", null), _define_property(_this, "_onThemeLinkLoad", void 0), _define_property(_this, "timeObs", null), _define_property(_this, "recentTemp", []), _define_property(_this, "recentEnergy", []), _define_property(_this, "recentTempEl", void 0), _define_property(_this, "recentEnergyEl", void 0), _define_property(_this, "xLabels", []), _define_property(_this, "showTemp", true), _define_property(_this, "showEnergy", true);
        return _this;
    }
    _create_class(MeteoDashboard, [
        {
            key: "createRenderRoot",
            value: function createRenderRoot() {
                return this;
            }
        },
        {
            key: "connectedCallback",
            value: function connectedCallback() {
                var _this = this;
                _get(_get_prototype_of(MeteoDashboard.prototype), "connectedCallback", this).call(this);
                this.cacheElements();
                this.wireToolbar();
                this.setupChart();
                this.loadFromPublic();
                this._onThemeChanged = function() {
                    return _this.rebuildChartForTheme();
                };
                window.addEventListener("theme-changed", this._onThemeChanged);
                var themeLink = document.querySelector('link#theme');
                if (themeLink && !this.themeObs) {
                    this._onThemeLinkLoad = function() {
                        return _this.rebuildChartForTheme();
                    };
                    this.themeObs = new MutationObserver(function() {
                        if (_this._onThemeLinkLoad) {
                            themeLink.removeEventListener('load', _this._onThemeLinkLoad);
                            themeLink.addEventListener('load', _this._onThemeLinkLoad, {
                                once: true
                            });
                        } else {
                            setTimeout(function() {
                                return _this.refreshChartColors();
                            }, 0);
                        }
                    });
                    this.themeObs.observe(themeLink, {
                        attributes: true,
                        attributeFilter: [
                            'href'
                        ]
                    });
                }
                if (!this.timeObs) {
                    var updateFromTime = function() {
                        var _this_currentTimeEl;
                        var t = (((_this_currentTimeEl = _this.currentTimeEl) === null || _this_currentTimeEl === void 0 ? void 0 : _this_currentTimeEl.textContent) || "").trim();
                        if (t && t.includes(":")) _this.setStatus("Sincronizado \xb7 ".concat(t));
                    };
                    this.timeObs = new MutationObserver(updateFromTime);
                    if (this.currentTimeEl) {
                        this.timeObs.observe(this.currentTimeEl, {
                            childList: true,
                            characterData: true,
                            subtree: true
                        });
                        updateFromTime();
                    }
                }
            }
        },
        {
            key: "disconnectedCallback",
            value: function disconnectedCallback() {
                _get(_get_prototype_of(MeteoDashboard.prototype), "disconnectedCallback", this).call(this);
                if (this._onThemeChanged) window.removeEventListener("theme-changed", this._onThemeChanged);
                if (this.themeObs) {
                    this.themeObs.disconnect();
                    this.themeObs = null;
                }
                if (this._onThemeLinkLoad) {
                    var themeLink = document.querySelector('link#theme');
                    if (themeLink) themeLink.removeEventListener('load', this._onThemeLinkLoad);
                    this._onThemeLinkLoad = undefined;
                }
                if (this.resizeObs) {
                    this.resizeObs.disconnect();
                    this.resizeObs = null;
                }
                if (this.timeObs) {
                    this.timeObs.disconnect();
                    this.timeObs = null;
                }
                this.pause();
            }
        },
        {
            key: "refreshChartColors",
            value: function refreshChartColors() {
                var _this_plot_data;
                if (!this.plot) return;
                var _$css = function(v, fb) {
                    return getComputedStyle(document.documentElement).getPropertyValue(v).trim() || fb;
                };
                var tempColor = _$css("--c-temp", "#7fb3ff");
                var tempFill = _$css("--c-temp-a", "rgba(127,179,255,.30)");
                var energyColor = _$css("--c-energy", "#73e0c7");
                var energyFill = _$css("--c-energy-a", "rgba(115,224,199,.30)");
                this.plot.setSeries(1, {
                    stroke: tempColor,
                    points: {
                        show: true,
                        size: 5,
                        stroke: tempColor,
                        fill: tempFill
                    }
                });
                this.plot.setSeries(2, {
                    stroke: energyColor,
                    points: {
                        show: true,
                        size: 5,
                        stroke: energyColor,
                        fill: energyFill
                    }
                });
                var hasData = Array.isArray((_this_plot_data = this.plot.data) === null || _this_plot_data === void 0 ? void 0 : _this_plot_data[0]) && this.plot.data[0].length > 0;
                if (hasData) this.plot.redraw();
            }
        },
        {
            key: "rebuildChartForTheme",
            value: function rebuildChartForTheme() {
                var hadPlot = !!this.plot;
                this.setupChart();
                if (hadPlot && this.perMinute.size > 0) {
                    this.updateChart();
                }
            }
        },
        {
            key: "cacheElements",
            value: function cacheElements() {
                this.statusEl = this.querySelector("#status");
                this.lastTempEl = this.querySelector("#last-temp");
                this.lastEnergyEl = this.querySelector("#last-energy");
                this.chartEl = this.querySelector("#chart");
                this.progressEl = this.querySelector("#progress");
                this.currentTimeEl = this.querySelector("#current-time");
                this.nextInEl = this.querySelector("#next-in");
                this.btnPlay = this.querySelector("#btnPlay");
                this.toggleTempBtn = this.querySelector("#toggleTemp");
                this.toggleEnergyBtn = this.querySelector("#toggleEnergy");
                this.recentTempEl = this.querySelector("#recent-temp");
                this.recentEnergyEl = this.querySelector("#recent-energy");
            }
        },
        {
            key: "wireToolbar",
            value: function wireToolbar() {
                var _this = this;
                var _this_toggleTempBtn, _this_toggleTempBtn1, _this_toggleEnergyBtn, _this_toggleEnergyBtn1, _this_toggleTempBtn2, _this_toggleEnergyBtn2;
                (_this_toggleTempBtn = this.toggleTempBtn) === null || _this_toggleTempBtn === void 0 ? void 0 : _this_toggleTempBtn.setAttribute("aria-pressed", String(this.showTemp));
                (_this_toggleTempBtn1 = this.toggleTempBtn) === null || _this_toggleTempBtn1 === void 0 ? void 0 : _this_toggleTempBtn1.setAttribute("title", this.showTemp ? "Ocultar temperatura" : "Mostrar temperatura");
                (_this_toggleEnergyBtn = this.toggleEnergyBtn) === null || _this_toggleEnergyBtn === void 0 ? void 0 : _this_toggleEnergyBtn.setAttribute("aria-pressed", String(this.showEnergy));
                (_this_toggleEnergyBtn1 = this.toggleEnergyBtn) === null || _this_toggleEnergyBtn1 === void 0 ? void 0 : _this_toggleEnergyBtn1.setAttribute("title", this.showEnergy ? "Ocultar energía" : "Mostrar energía");
                (_this_toggleTempBtn2 = this.toggleTempBtn) === null || _this_toggleTempBtn2 === void 0 ? void 0 : _this_toggleTempBtn2.addEventListener("click", function() {
                    _this.showTemp = !_this.showTemp;
                    _this.toggleTempBtn.setAttribute("aria-pressed", String(_this.showTemp));
                    _this.toggleTempBtn.setAttribute("title", _this.showTemp ? "Ocultar temperatura" : "Mostrar temperatura");
                    _this.updateChart();
                });
                (_this_toggleEnergyBtn2 = this.toggleEnergyBtn) === null || _this_toggleEnergyBtn2 === void 0 ? void 0 : _this_toggleEnergyBtn2.addEventListener("click", function() {
                    _this.showEnergy = !_this.showEnergy;
                    _this.toggleEnergyBtn.setAttribute("aria-pressed", String(_this.showEnergy));
                    _this.toggleEnergyBtn.setAttribute("title", _this.showEnergy ? "Ocultar energía" : "Mostrar energía");
                    _this.updateChart();
                });
                this.addEventListener("keydown", function(e) {
                    if (e.code === "Space") {
                        e.preventDefault();
                        _this.playing ? _this.pause() : _this.startAuto();
                    }
                });
            }
        },
        {
            key: "loadFromPublic",
            value: function loadFromPublic() {
                return _async_to_generator(function() {
                    var _this, _temperature_values, res, text, doc, temperature, energy, norm, err;
                    return _ts_generator(this, function(_state) {
                        switch(_state.label){
                            case 0:
                                _this = this;
                                _state.label = 1;
                            case 1:
                                _state.trys.push([
                                    1,
                                    4,
                                    ,
                                    5
                                ]);
                                this.setStatus("Cargando /data.yml …");
                                return [
                                    4,
                                    fetch("./data.yml", {
                                        cache: "no-store"
                                    })
                                ];
                            case 2:
                                res = _state.sent();
                                if (!res.ok) throw new Error("HTTP ".concat(res.status));
                                return [
                                    4,
                                    res.text()
                                ];
                            case 3:
                                text = _state.sent();
                                doc = js_yaml__WEBPACK_IMPORTED_MODULE_2__["default"].load(text);
                                temperature = doc.temperature || doc.temperatura;
                                if (!(temperature === null || temperature === void 0 ? void 0 : (_temperature_values = temperature.values) === null || _temperature_values === void 0 ? void 0 : _temperature_values.length)) throw new Error("No hay temperature.values en el YAML");
                                energy = doc.energy || doc.energia || doc.power || doc.potencia || null;
                                norm = function(s) {
                                    return {
                                        unit: (s.unit || "K").trim(),
                                        values: s.values.map(function(v) {
                                            return {
                                                time: v.time,
                                                value: Number(v.value)
                                            };
                                        }).sort(function(a, b) {
                                            return _this.toSeconds(a.time) - _this.toSeconds(b.time);
                                        })
                                    };
                                };
                                this.data = {
                                    temperature: norm(temperature),
                                    energy: energy ? norm(energy) : null
                                };
                                this.pointer = 0;
                                this.perMinute.clear();
                                this.updateChart();
                                this.recentTemp = [];
                                this.recentEnergy = [];
                                this.renderRecent();
                                this.syncPointerToNow();
                                this.startAuto();
                                return [
                                    3,
                                    5
                                ];
                            case 4:
                                err = _state.sent();
                                console.error(err);
                                this.setStatus("Error cargando data.yml: " + err.message);
                                return [
                                    3,
                                    5
                                ];
                            case 5:
                                return [
                                    2
                                ];
                        }
                    });
                }).call(this);
            }
        },
        {
            key: "syncPointerToNow",
            value: function syncPointerToNow() {
                if (!this.data) return;
                var now = new Date();
                var nowStr = "".concat(String(now.getHours()).padStart(2, "0"), ":").concat(String(now.getMinutes()).padStart(2, "0"), ":").concat(String(now.getSeconds()).padStart(2, "0"));
                var nowSec = this.toSeconds(nowStr);
                var arr = this.data.temperature.values;
                var idx = 0;
                for(var i = 0; i < arr.length; i++){
                    var s = this.toSeconds(arr[i].time);
                    if (s <= nowSec) idx = i;
                    else break;
                }
                this.pointer = idx;
                this.revealCurrentOnly();
                this.setStatus("Sincronizado \xb7 ".concat(nowStr));
                this.setNextAt();
            }
        },
        {
            key: "setNextAt",
            value: function setNextAt() {
                var _this = this;
                this.nextAt = Date.now() + 5000;
                if (this.secondTimer) clearInterval(this.secondTimer);
                this.secondTimer = window.setInterval(function() {
                    var ms = _this.nextAt - Date.now();
                    var s = Math.max(0, Math.ceil(ms / 1000));
                    _this.nextInEl.textContent = String(s);
                    if (ms <= 0) clearInterval(_this.secondTimer);
                }, 200);
            }
        },
        {
            key: "startAuto",
            value: function startAuto() {
                var _this = this;
                var _this_btnPlay;
                if (this.timer) return;
                this.playing = true;
                (_this_btnPlay = this.btnPlay) === null || _this_btnPlay === void 0 ? void 0 : _this_btnPlay.setAttribute("aria-pressed", "true");
                this.timer = window.setInterval(function() {
                    _this.revealNext();
                    _this.setNextAt();
                }, 5000);
            }
        },
        {
            key: "pause",
            value: function pause() {
                var _this_btnPlay;
                this.playing = false;
                (_this_btnPlay = this.btnPlay) === null || _this_btnPlay === void 0 ? void 0 : _this_btnPlay.setAttribute("aria-pressed", "false");
                if (this.timer) {
                    window.clearInterval(this.timer);
                    this.timer = null;
                }
                if (this.secondTimer) {
                    window.clearInterval(this.secondTimer);
                    this.secondTimer = null;
                }
            }
        },
        {
            key: "revealCurrentOnly",
            value: function revealCurrentOnly() {
                if (!this.data) return;
                var t = this.data.temperature.values[this.pointer];
                var e = this.data.energy ? this.data.energy.values[this.pointer] : null;
                var tempC = this.toCelsius(t.value, this.data.temperature.unit);
                var energyKWh = e ? this.toKWh(e.value, this.data.energy.unit) : null;
                this.lastTempEl.textContent = this.fmt2(tempC);
                if (energyKWh !== null) this.lastEnergyEl.textContent = this.fmt2(energyKWh);
                this.currentTimeEl.textContent = t.time;
                this.progressEl.textContent = "".concat(this.pointer + 1, " / ").concat(this.data.temperature.values.length);
                this.pushRecent('temp', t.time, tempC);
                this.pushRecent('energy', t.time, energyKWh);
                this.renderRecent();
                var minuteKey = t.time.slice(0, 5);
                var bucket = this.perMinute.get(minuteKey) || {
                    t: [],
                    e: []
                };
                bucket.t.push(tempC);
                if (energyKWh !== null) bucket.e.push(energyKWh);
                this.perMinute.set(minuteKey, bucket);
                this.updateChart();
            }
        },
        {
            key: "revealNext",
            value: function revealNext() {
                if (!this.data) return;
                if (this.pointer >= this.data.temperature.values.length - 1) {
                    this.pause();
                    this.setStatus("Fin del dataset");
                    return;
                }
                this.pointer++;
                this.revealCurrentOnly();
            }
        },
        {
            key: "setupChart",
            value: function setupChart() {
                var _this = this;
                if (this.plot) {
                    this.plot.destroy();
                    this.plot = null;
                }
                var getW = function() {
                    var w = Math.floor(_this.chartEl.getBoundingClientRect().width);
                    if (w && w > 0) return w;
                    var pw = _this.chartEl.parentElement ? Math.floor(_this.chartEl.parentElement.getBoundingClientRect().width) : 0;
                    return pw > 0 ? pw : 600;
                };
                var _$css = function(v, fallback) {
                    return getComputedStyle(document.documentElement).getPropertyValue(v).trim() || fallback;
                };
                var tempColor = _$css("--c-temp", "#7fb3ff");
                var tempFill = _$css("--c-temp-a", "rgba(127,179,255,.30)");
                var energyColor = _$css("--c-energy", "#73e0c7");
                var energyFill = _$css("--c-energy-a", "rgba(115,224,199,.30)");
                var axisColor = _$css("--muted", "rgba(190,205,255,.90)");
                var gridColor = _$css("--border", "rgba(130,150,210,.25)");
                var opts = {
                    title: "",
                    width: getW(),
                    height: 360,
                    pxAlign: 0.5,
                    scales: {
                        x: {
                            time: false
                        },
                        temp: {
                            auto: true
                        },
                        energy: {
                            auto: true
                        }
                    },
                    axes: [
                        {
                            stroke: axisColor,
                            grid: {
                                show: true,
                                stroke: gridColor
                            },
                            values: function(_u, vals) {
                                return vals.map(function(v) {
                                    var _this_xLabels_Math_round;
                                    return (_this_xLabels_Math_round = _this.xLabels[Math.round(v)]) !== null && _this_xLabels_Math_round !== void 0 ? _this_xLabels_Math_round : "";
                                });
                            }
                        },
                        {
                            scale: "temp",
                            label: "°C",
                            stroke: axisColor,
                            size: 48,
                            gap: 6
                        },
                        {
                            side: 1,
                            scale: "energy",
                            label: "kWh",
                            stroke: axisColor,
                            size: 56,
                            gap: 6
                        }
                    ],
                    series: [
                        {},
                        {
                            label: "Temp (°C)",
                            scale: "temp",
                            points: {
                                show: true,
                                size: 5,
                                stroke: tempColor,
                                fill: tempFill
                            },
                            stroke: tempColor,
                            width: 2
                        },
                        {
                            label: "Energía (kWh)",
                            scale: "energy",
                            points: {
                                show: true,
                                size: 5,
                                stroke: energyColor,
                                fill: energyFill
                            },
                            stroke: energyColor,
                            width: 2
                        }
                    ],
                    cursor: {
                        drag: {
                            x: false,
                            y: false
                        },
                        focus: {
                            prox: 24
                        }
                    },
                    legend: {
                        show: false
                    }
                };
                this.plot = new uplot__WEBPACK_IMPORTED_MODULE_1__["default"](opts, [
                    [],
                    [],
                    []
                ], this.chartEl);
                this.refreshChartColors();
                if (!this.resizeObs) {
                    this.resizeObs = new ResizeObserver(function() {
                        if (_this.plot) _this.plot.setSize({
                            width: getW(),
                            height: 360
                        });
                    });
                    this.resizeObs.observe(this.chartEl);
                }
            }
        },
        {
            key: "updateChart",
            value: function updateChart() {
                var _this = this;
                if (!this.plot) this.setupChart();
                var keys = Array.from(this.perMinute.keys()).sort();
                this.xLabels = keys;
                var x = keys.map(function(_, i) {
                    return i;
                });
                var tSeries = keys.map(function(k) {
                    return _this.mean(_this.perMinute.get(k).t);
                });
                var eSeries = keys.map(function(k) {
                    return _this.mean(_this.perMinute.get(k).e);
                });
                var tData = this.showTemp ? tSeries : tSeries.map(function() {
                    return null;
                });
                var eData = this.showEnergy ? eSeries : eSeries.map(function() {
                    return null;
                });
                this.plot.setData([
                    x,
                    tData,
                    eData
                ]);
            }
        },
        {
            key: "setStatus",
            value: function setStatus(msg) {
                this.statusEl.textContent = msg;
            }
        },
        {
            key: "pushRecent",
            value: function pushRecent(kind, time, value) {
                if (value === null || Number.isNaN(value)) return;
                var arr = kind === 'temp' ? this.recentTemp : this.recentEnergy;
                arr.unshift({
                    time: time,
                    value: value
                });
                if (arr.length > 5) arr.length = 5;
            }
        },
        {
            key: "renderRecent",
            value: function renderRecent() {
                var _this = this;
                if (!this.recentTempEl || !this.recentEnergyEl) return;
                var tpl = function(arr, unit) {
                    return arr.map(function(e) {
                        return '<li><span class="t">'.concat(e.time, '</span><span class="v">').concat(_this.fmt2(e.value), ' <span class="unit">').concat(unit, "</span></span></li>");
                    }).join("");
                };
                this.recentTempEl.innerHTML = tpl(this.recentTemp, "°C");
                this.recentEnergyEl.innerHTML = tpl(this.recentEnergy, "kWh");
            }
        },
        {
            key: "toSeconds",
            value: function toSeconds(t) {
                var _t_split_map = _sliced_to_array(t.split(":").map(Number), 3), H = _t_split_map[0], M = _t_split_map[1], S = _t_split_map[2];
                return H * 3600 + M * 60 + S;
            }
        },
        {
            key: "toCelsius",
            value: function toCelsius(value, unit) {
                var u = (unit || "").toLowerCase();
                if (u === "dk") return value / 10 - 273.15;
                if (u === "k") return value - 273.15;
                if (u.includes("c")) return value;
                return value - 273.15;
            }
        },
        {
            key: "toKWh",
            value: function toKWh(value, unit) {
                var u = (unit || "").toLowerCase();
                if (u === "kwh") return value;
                if (u === "wh") return value / 1000;
                return value;
            }
        },
        {
            key: "mean",
            value: function mean(arr) {
                if (!arr || arr.length === 0) return null;
                return arr.reduce(function(a, b) {
                    return a + b;
                }, 0) / arr.length;
            }
        },
        {
            key: "fmt2",
            value: function fmt2(n) {
                if (n === null || Number.isNaN(n)) return "–";
                return (Math.round(n * 100) / 100).toFixed(2);
            }
        }
    ]);
    return MeteoDashboard;
}(lit__WEBPACK_IMPORTED_MODULE_0__.LitElement);
_define_property(MeteoDashboard, "styles", (0,lit__WEBPACK_IMPORTED_MODULE_0__.css)(_templateObject()));
customElements.define("meteo-dashboard", MeteoDashboard);


}),
"./node_modules/.pnpm/@lit+reactive-element@2.1.1/node_modules/@lit/reactive-element/development/css-tag.js": 
/*!******************************************************************************************************************!*\
  !*** ./node_modules/.pnpm/@lit+reactive-element@2.1.1/node_modules/@lit/reactive-element/development/css-tag.js ***!
  \******************************************************************************************************************/
(function (__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  CSSResult: () => (CSSResult),
  adoptStyles: () => (adoptStyles),
  css: () => (css),
  getCompatibleStyle: () => (getCompatibleStyle),
  supportsAdoptingStyleSheets: () => (supportsAdoptingStyleSheets),
  unsafeCSS: () => (unsafeCSS)
});
/**
 * @license
 * Copyright 2019 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const NODE_MODE = false;
// Allows minifiers to rename references to globalThis
const global = globalThis;
/**
 * Whether the current browser supports `adoptedStyleSheets`.
 */
const supportsAdoptingStyleSheets = global.ShadowRoot &&
    (global.ShadyCSS === undefined || global.ShadyCSS.nativeShadow) &&
    'adoptedStyleSheets' in Document.prototype &&
    'replace' in CSSStyleSheet.prototype;
const constructionToken = Symbol();
const cssTagCache = new WeakMap();
/**
 * A container for a string of CSS text, that may be used to create a CSSStyleSheet.
 *
 * CSSResult is the return value of `css`-tagged template literals and
 * `unsafeCSS()`. In order to ensure that CSSResults are only created via the
 * `css` tag and `unsafeCSS()`, CSSResult cannot be constructed directly.
 */
class CSSResult {
    constructor(cssText, strings, safeToken) {
        // This property needs to remain unminified.
        this['_$cssResult$'] = true;
        if (safeToken !== constructionToken) {
            throw new Error('CSSResult is not constructable. Use `unsafeCSS` or `css` instead.');
        }
        this.cssText = cssText;
        this._strings = strings;
    }
    // This is a getter so that it's lazy. In practice, this means stylesheets
    // are not created until the first element instance is made.
    get styleSheet() {
        // If `supportsAdoptingStyleSheets` is true then we assume CSSStyleSheet is
        // constructable.
        let styleSheet = this._styleSheet;
        const strings = this._strings;
        if (supportsAdoptingStyleSheets && styleSheet === undefined) {
            const cacheable = strings !== undefined && strings.length === 1;
            if (cacheable) {
                styleSheet = cssTagCache.get(strings);
            }
            if (styleSheet === undefined) {
                (this._styleSheet = styleSheet = new CSSStyleSheet()).replaceSync(this.cssText);
                if (cacheable) {
                    cssTagCache.set(strings, styleSheet);
                }
            }
        }
        return styleSheet;
    }
    toString() {
        return this.cssText;
    }
}
const textFromCSSResult = (value) => {
    // This property needs to remain unminified.
    if (value['_$cssResult$'] === true) {
        return value.cssText;
    }
    else if (typeof value === 'number') {
        return value;
    }
    else {
        throw new Error(`Value passed to 'css' function must be a 'css' function result: ` +
            `${value}. Use 'unsafeCSS' to pass non-literal values, but take care ` +
            `to ensure page security.`);
    }
};
/**
 * Wrap a value for interpolation in a {@linkcode css} tagged template literal.
 *
 * This is unsafe because untrusted CSS text can be used to phone home
 * or exfiltrate data to an attacker controlled site. Take care to only use
 * this with trusted input.
 */
const unsafeCSS = (value) => new CSSResult(typeof value === 'string' ? value : String(value), undefined, constructionToken);
/**
 * A template literal tag which can be used with LitElement's
 * {@linkcode LitElement.styles} property to set element styles.
 *
 * For security reasons, only literal string values and number may be used in
 * embedded expressions. To incorporate non-literal values {@linkcode unsafeCSS}
 * may be used inside an expression.
 */
const css = (strings, ...values) => {
    const cssText = strings.length === 1
        ? strings[0]
        : values.reduce((acc, v, idx) => acc + textFromCSSResult(v) + strings[idx + 1], strings[0]);
    return new CSSResult(cssText, strings, constructionToken);
};
/**
 * Applies the given styles to a `shadowRoot`. When Shadow DOM is
 * available but `adoptedStyleSheets` is not, styles are appended to the
 * `shadowRoot` to [mimic the native feature](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot/adoptedStyleSheets).
 * Note, when shimming is used, any styles that are subsequently placed into
 * the shadowRoot should be placed *before* any shimmed adopted styles. This
 * will match spec behavior that gives adopted sheets precedence over styles in
 * shadowRoot.
 */
const adoptStyles = (renderRoot, styles) => {
    if (supportsAdoptingStyleSheets) {
        renderRoot.adoptedStyleSheets = styles.map((s) => s instanceof CSSStyleSheet ? s : s.styleSheet);
    }
    else {
        for (const s of styles) {
            const style = document.createElement('style');
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            const nonce = global['litNonce'];
            if (nonce !== undefined) {
                style.setAttribute('nonce', nonce);
            }
            style.textContent = s.cssText;
            renderRoot.appendChild(style);
        }
    }
};
const cssResultFromStyleSheet = (sheet) => {
    let cssText = '';
    for (const rule of sheet.cssRules) {
        cssText += rule.cssText;
    }
    return unsafeCSS(cssText);
};
const getCompatibleStyle = supportsAdoptingStyleSheets ||
    (NODE_MODE && global.CSSStyleSheet === undefined)
    ? (s) => s
    : (s) => s instanceof CSSStyleSheet ? cssResultFromStyleSheet(s) : s;
//# sourceMappingURL=css-tag.js.map

}),
"./node_modules/.pnpm/@lit+reactive-element@2.1.1/node_modules/@lit/reactive-element/development/reactive-element.js": 
/*!***************************************************************************************************************************!*\
  !*** ./node_modules/.pnpm/@lit+reactive-element@2.1.1/node_modules/@lit/reactive-element/development/reactive-element.js ***!
  \***************************************************************************************************************************/
(function (__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  CSSResult: () => (/* reexport safe */ _css_tag_js__WEBPACK_IMPORTED_MODULE_0__.CSSResult),
  ReactiveElement: () => (ReactiveElement),
  adoptStyles: () => (/* reexport safe */ _css_tag_js__WEBPACK_IMPORTED_MODULE_0__.adoptStyles),
  css: () => (/* reexport safe */ _css_tag_js__WEBPACK_IMPORTED_MODULE_0__.css),
  defaultConverter: () => (defaultConverter),
  getCompatibleStyle: () => (/* reexport safe */ _css_tag_js__WEBPACK_IMPORTED_MODULE_0__.getCompatibleStyle),
  notEqual: () => (notEqual),
  supportsAdoptingStyleSheets: () => (/* reexport safe */ _css_tag_js__WEBPACK_IMPORTED_MODULE_0__.supportsAdoptingStyleSheets),
  unsafeCSS: () => (/* reexport safe */ _css_tag_js__WEBPACK_IMPORTED_MODULE_0__.unsafeCSS)
});
/* ESM import */var _css_tag_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./css-tag.js */ "./node_modules/.pnpm/@lit+reactive-element@2.1.1/node_modules/@lit/reactive-element/development/css-tag.js");
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
/**
 * Use this module if you want to create your own base class extending
 * {@link ReactiveElement}.
 * @packageDocumentation
 */

// In the Node build, this import will be injected by Rollup:
// import {HTMLElement, customElements} from '@lit-labs/ssr-dom-shim';

// TODO (justinfagnani): Add `hasOwn` here when we ship ES2022
const { is, defineProperty, getOwnPropertyDescriptor, getOwnPropertyNames, getOwnPropertySymbols, getPrototypeOf, } = Object;
const NODE_MODE = false;
// Lets a minifier replace globalThis references with a minified name
const global = globalThis;
if (NODE_MODE) {
    global.customElements ??= customElements;
}
const DEV_MODE = true;
let issueWarning;
const trustedTypes = global
    .trustedTypes;
// Temporary workaround for https://crbug.com/993268
// Currently, any attribute starting with "on" is considered to be a
// TrustedScript source. Such boolean attributes must be set to the equivalent
// trusted emptyScript value.
const emptyStringForBooleanAttribute = trustedTypes
    ? trustedTypes.emptyScript
    : '';
const polyfillSupport = DEV_MODE
    ? global.reactiveElementPolyfillSupportDevMode
    : global.reactiveElementPolyfillSupport;
if (DEV_MODE) {
    // Ensure warnings are issued only 1x, even if multiple versions of Lit
    // are loaded.
    global.litIssuedWarnings ??= new Set();
    /**
     * Issue a warning if we haven't already, based either on `code` or `warning`.
     * Warnings are disabled automatically only by `warning`; disabling via `code`
     * can be done by users.
     */
    issueWarning = (code, warning) => {
        warning += ` See https://lit.dev/msg/${code} for more information.`;
        if (!global.litIssuedWarnings.has(warning) &&
            !global.litIssuedWarnings.has(code)) {
            console.warn(warning);
            global.litIssuedWarnings.add(warning);
        }
    };
    queueMicrotask(() => {
        issueWarning('dev-mode', `Lit is in dev mode. Not recommended for production!`);
        // Issue polyfill support warning.
        if (global.ShadyDOM?.inUse && polyfillSupport === undefined) {
            issueWarning('polyfill-support-missing', `Shadow DOM is being polyfilled via \`ShadyDOM\` but ` +
                `the \`polyfill-support\` module has not been loaded.`);
        }
    });
}
/**
 * Useful for visualizing and logging insights into what the Lit template system is doing.
 *
 * Compiled out of prod mode builds.
 */
const debugLogEvent = DEV_MODE
    ? (event) => {
        const shouldEmit = global
            .emitLitDebugLogEvents;
        if (!shouldEmit) {
            return;
        }
        global.dispatchEvent(new CustomEvent('lit-debug', {
            detail: event,
        }));
    }
    : undefined;
/*
 * When using Closure Compiler, JSCompiler_renameProperty(property, object) is
 * replaced at compile time by the munged name for object[property]. We cannot
 * alias this function, so we have to use a small shim that has the same
 * behavior when not compiling.
 */
/*@__INLINE__*/
const JSCompiler_renameProperty = (prop, _obj) => prop;
const defaultConverter = {
    toAttribute(value, type) {
        switch (type) {
            case Boolean:
                value = value ? emptyStringForBooleanAttribute : null;
                break;
            case Object:
            case Array:
                // if the value is `null` or `undefined` pass this through
                // to allow removing/no change behavior.
                value = value == null ? value : JSON.stringify(value);
                break;
        }
        return value;
    },
    fromAttribute(value, type) {
        let fromValue = value;
        switch (type) {
            case Boolean:
                fromValue = value !== null;
                break;
            case Number:
                fromValue = value === null ? null : Number(value);
                break;
            case Object:
            case Array:
                // Do *not* generate exception when invalid JSON is set as elements
                // don't normally complain on being mis-configured.
                // TODO(sorvell): Do generate exception in *dev mode*.
                try {
                    // Assert to adhere to Bazel's "must type assert JSON parse" rule.
                    fromValue = JSON.parse(value);
                }
                catch (e) {
                    fromValue = null;
                }
                break;
        }
        return fromValue;
    },
};
/**
 * Change function that returns true if `value` is different from `oldValue`.
 * This method is used as the default for a property's `hasChanged` function.
 */
const notEqual = (value, old) => !is(value, old);
const defaultPropertyDeclaration = {
    attribute: true,
    type: String,
    converter: defaultConverter,
    reflect: false,
    useDefault: false,
    hasChanged: notEqual,
};
// Ensure metadata is enabled. TypeScript does not polyfill
// Symbol.metadata, so we must ensure that it exists.
Symbol.metadata ??= Symbol('metadata');
// Map from a class's metadata object to property options
// Note that we must use nullish-coalescing assignment so that we only use one
// map even if we load multiple version of this module.
global.litPropertyMetadata ??= new WeakMap();
/**
 * Base element class which manages element properties and attributes. When
 * properties change, the `update` method is asynchronously called. This method
 * should be supplied by subclasses to render updates as desired.
 * @noInheritDoc
 */
class ReactiveElement
// In the Node build, this `extends` clause will be substituted with
// `(globalThis.HTMLElement ?? HTMLElement)`.
//
// This way, we will first prefer any global `HTMLElement` polyfill that the
// user has assigned, and then fall back to the `HTMLElement` shim which has
// been imported (see note at the top of this file about how this import is
// generated by Rollup). Note that the `HTMLElement` variable has been
// shadowed by this import, so it no longer refers to the global.
 extends HTMLElement {
    /**
     * Adds an initializer function to the class that is called during instance
     * construction.
     *
     * This is useful for code that runs against a `ReactiveElement`
     * subclass, such as a decorator, that needs to do work for each
     * instance, such as setting up a `ReactiveController`.
     *
     * ```ts
     * const myDecorator = (target: typeof ReactiveElement, key: string) => {
     *   target.addInitializer((instance: ReactiveElement) => {
     *     // This is run during construction of the element
     *     new MyController(instance);
     *   });
     * }
     * ```
     *
     * Decorating a field will then cause each instance to run an initializer
     * that adds a controller:
     *
     * ```ts
     * class MyElement extends LitElement {
     *   @myDecorator foo;
     * }
     * ```
     *
     * Initializers are stored per-constructor. Adding an initializer to a
     * subclass does not add it to a superclass. Since initializers are run in
     * constructors, initializers will run in order of the class hierarchy,
     * starting with superclasses and progressing to the instance's class.
     *
     * @nocollapse
     */
    static addInitializer(initializer) {
        this.__prepare();
        (this._initializers ??= []).push(initializer);
    }
    /**
     * Returns a list of attributes corresponding to the registered properties.
     * @nocollapse
     * @category attributes
     */
    static get observedAttributes() {
        // Ensure we've created all properties
        this.finalize();
        // this.__attributeToPropertyMap is only undefined after finalize() in
        // ReactiveElement itself. ReactiveElement.observedAttributes is only
        // accessed with ReactiveElement as the receiver when a subclass or mixin
        // calls super.observedAttributes
        return (this.__attributeToPropertyMap && [...this.__attributeToPropertyMap.keys()]);
    }
    /**
     * Creates a property accessor on the element prototype if one does not exist
     * and stores a {@linkcode PropertyDeclaration} for the property with the
     * given options. The property setter calls the property's `hasChanged`
     * property option or uses a strict identity check to determine whether or not
     * to request an update.
     *
     * This method may be overridden to customize properties; however,
     * when doing so, it's important to call `super.createProperty` to ensure
     * the property is setup correctly. This method calls
     * `getPropertyDescriptor` internally to get a descriptor to install.
     * To customize what properties do when they are get or set, override
     * `getPropertyDescriptor`. To customize the options for a property,
     * implement `createProperty` like this:
     *
     * ```ts
     * static createProperty(name, options) {
     *   options = Object.assign(options, {myOption: true});
     *   super.createProperty(name, options);
     * }
     * ```
     *
     * @nocollapse
     * @category properties
     */
    static createProperty(name, options = defaultPropertyDeclaration) {
        // If this is a state property, force the attribute to false.
        if (options.state) {
            options.attribute = false;
        }
        this.__prepare();
        // Whether this property is wrapping accessors.
        // Helps control the initial value change and reflection logic.
        if (this.prototype.hasOwnProperty(name)) {
            options = Object.create(options);
            options.wrapped = true;
        }
        this.elementProperties.set(name, options);
        if (!options.noAccessor) {
            const key = DEV_MODE
                ? // Use Symbol.for in dev mode to make it easier to maintain state
                    // when doing HMR.
                    Symbol.for(`${String(name)} (@property() cache)`)
                : Symbol();
            const descriptor = this.getPropertyDescriptor(name, key, options);
            if (descriptor !== undefined) {
                defineProperty(this.prototype, name, descriptor);
            }
        }
    }
    /**
     * Returns a property descriptor to be defined on the given named property.
     * If no descriptor is returned, the property will not become an accessor.
     * For example,
     *
     * ```ts
     * class MyElement extends LitElement {
     *   static getPropertyDescriptor(name, key, options) {
     *     const defaultDescriptor =
     *         super.getPropertyDescriptor(name, key, options);
     *     const setter = defaultDescriptor.set;
     *     return {
     *       get: defaultDescriptor.get,
     *       set(value) {
     *         setter.call(this, value);
     *         // custom action.
     *       },
     *       configurable: true,
     *       enumerable: true
     *     }
     *   }
     * }
     * ```
     *
     * @nocollapse
     * @category properties
     */
    static getPropertyDescriptor(name, key, options) {
        const { get, set } = getOwnPropertyDescriptor(this.prototype, name) ?? {
            get() {
                return this[key];
            },
            set(v) {
                this[key] = v;
            },
        };
        if (DEV_MODE && get == null) {
            if ('value' in (getOwnPropertyDescriptor(this.prototype, name) ?? {})) {
                throw new Error(`Field ${JSON.stringify(String(name))} on ` +
                    `${this.name} was declared as a reactive property ` +
                    `but it's actually declared as a value on the prototype. ` +
                    `Usually this is due to using @property or @state on a method.`);
            }
            issueWarning('reactive-property-without-getter', `Field ${JSON.stringify(String(name))} on ` +
                `${this.name} was declared as a reactive property ` +
                `but it does not have a getter. This will be an error in a ` +
                `future version of Lit.`);
        }
        return {
            get,
            set(value) {
                const oldValue = get?.call(this);
                set?.call(this, value);
                this.requestUpdate(name, oldValue, options);
            },
            configurable: true,
            enumerable: true,
        };
    }
    /**
     * Returns the property options associated with the given property.
     * These options are defined with a `PropertyDeclaration` via the `properties`
     * object or the `@property` decorator and are registered in
     * `createProperty(...)`.
     *
     * Note, this method should be considered "final" and not overridden. To
     * customize the options for a given property, override
     * {@linkcode createProperty}.
     *
     * @nocollapse
     * @final
     * @category properties
     */
    static getPropertyOptions(name) {
        return this.elementProperties.get(name) ?? defaultPropertyDeclaration;
    }
    /**
     * Initializes static own properties of the class used in bookkeeping
     * for element properties, initializers, etc.
     *
     * Can be called multiple times by code that needs to ensure these
     * properties exist before using them.
     *
     * This method ensures the superclass is finalized so that inherited
     * property metadata can be copied down.
     * @nocollapse
     */
    static __prepare() {
        if (this.hasOwnProperty(JSCompiler_renameProperty('elementProperties', this))) {
            // Already prepared
            return;
        }
        // Finalize any superclasses
        const superCtor = getPrototypeOf(this);
        superCtor.finalize();
        // Create own set of initializers for this class if any exist on the
        // superclass and copy them down. Note, for a small perf boost, avoid
        // creating initializers unless needed.
        if (superCtor._initializers !== undefined) {
            this._initializers = [...superCtor._initializers];
        }
        // Initialize elementProperties from the superclass
        this.elementProperties = new Map(superCtor.elementProperties);
    }
    /**
     * Finishes setting up the class so that it's ready to be registered
     * as a custom element and instantiated.
     *
     * This method is called by the ReactiveElement.observedAttributes getter.
     * If you override the observedAttributes getter, you must either call
     * super.observedAttributes to trigger finalization, or call finalize()
     * yourself.
     *
     * @nocollapse
     */
    static finalize() {
        if (this.hasOwnProperty(JSCompiler_renameProperty('finalized', this))) {
            return;
        }
        this.finalized = true;
        this.__prepare();
        // Create properties from the static properties block:
        if (this.hasOwnProperty(JSCompiler_renameProperty('properties', this))) {
            const props = this.properties;
            const propKeys = [
                ...getOwnPropertyNames(props),
                ...getOwnPropertySymbols(props),
            ];
            for (const p of propKeys) {
                this.createProperty(p, props[p]);
            }
        }
        // Create properties from standard decorator metadata:
        const metadata = this[Symbol.metadata];
        if (metadata !== null) {
            const properties = litPropertyMetadata.get(metadata);
            if (properties !== undefined) {
                for (const [p, options] of properties) {
                    this.elementProperties.set(p, options);
                }
            }
        }
        // Create the attribute-to-property map
        this.__attributeToPropertyMap = new Map();
        for (const [p, options] of this.elementProperties) {
            const attr = this.__attributeNameForProperty(p, options);
            if (attr !== undefined) {
                this.__attributeToPropertyMap.set(attr, p);
            }
        }
        this.elementStyles = this.finalizeStyles(this.styles);
        if (DEV_MODE) {
            if (this.hasOwnProperty('createProperty')) {
                issueWarning('no-override-create-property', 'Overriding ReactiveElement.createProperty() is deprecated. ' +
                    'The override will not be called with standard decorators');
            }
            if (this.hasOwnProperty('getPropertyDescriptor')) {
                issueWarning('no-override-get-property-descriptor', 'Overriding ReactiveElement.getPropertyDescriptor() is deprecated. ' +
                    'The override will not be called with standard decorators');
            }
        }
    }
    /**
     * Takes the styles the user supplied via the `static styles` property and
     * returns the array of styles to apply to the element.
     * Override this method to integrate into a style management system.
     *
     * Styles are deduplicated preserving the _last_ instance in the list. This
     * is a performance optimization to avoid duplicated styles that can occur
     * especially when composing via subclassing. The last item is kept to try
     * to preserve the cascade order with the assumption that it's most important
     * that last added styles override previous styles.
     *
     * @nocollapse
     * @category styles
     */
    static finalizeStyles(styles) {
        const elementStyles = [];
        if (Array.isArray(styles)) {
            // Dedupe the flattened array in reverse order to preserve the last items.
            // Casting to Array<unknown> works around TS error that
            // appears to come from trying to flatten a type CSSResultArray.
            const set = new Set(styles.flat(Infinity).reverse());
            // Then preserve original order by adding the set items in reverse order.
            for (const s of set) {
                elementStyles.unshift((0,_css_tag_js__WEBPACK_IMPORTED_MODULE_0__.getCompatibleStyle)(s));
            }
        }
        else if (styles !== undefined) {
            elementStyles.push((0,_css_tag_js__WEBPACK_IMPORTED_MODULE_0__.getCompatibleStyle)(styles));
        }
        return elementStyles;
    }
    /**
     * Returns the property name for the given attribute `name`.
     * @nocollapse
     */
    static __attributeNameForProperty(name, options) {
        const attribute = options.attribute;
        return attribute === false
            ? undefined
            : typeof attribute === 'string'
                ? attribute
                : typeof name === 'string'
                    ? name.toLowerCase()
                    : undefined;
    }
    constructor() {
        super();
        this.__instanceProperties = undefined;
        /**
         * True if there is a pending update as a result of calling `requestUpdate()`.
         * Should only be read.
         * @category updates
         */
        this.isUpdatePending = false;
        /**
         * Is set to `true` after the first update. The element code cannot assume
         * that `renderRoot` exists before the element `hasUpdated`.
         * @category updates
         */
        this.hasUpdated = false;
        /**
         * Name of currently reflecting property
         */
        this.__reflectingProperty = null;
        this.__initialize();
    }
    /**
     * Internal only override point for customizing work done when elements
     * are constructed.
     */
    __initialize() {
        this.__updatePromise = new Promise((res) => (this.enableUpdating = res));
        this._$changedProperties = new Map();
        // This enqueues a microtask that must run before the first update, so it
        // must be called before requestUpdate()
        this.__saveInstanceProperties();
        // ensures first update will be caught by an early access of
        // `updateComplete`
        this.requestUpdate();
        this.constructor._initializers?.forEach((i) => i(this));
    }
    /**
     * Registers a `ReactiveController` to participate in the element's reactive
     * update cycle. The element automatically calls into any registered
     * controllers during its lifecycle callbacks.
     *
     * If the element is connected when `addController()` is called, the
     * controller's `hostConnected()` callback will be immediately called.
     * @category controllers
     */
    addController(controller) {
        (this.__controllers ??= new Set()).add(controller);
        // If a controller is added after the element has been connected,
        // call hostConnected. Note, re-using existence of `renderRoot` here
        // (which is set in connectedCallback) to avoid the need to track a
        // first connected state.
        if (this.renderRoot !== undefined && this.isConnected) {
            controller.hostConnected?.();
        }
    }
    /**
     * Removes a `ReactiveController` from the element.
     * @category controllers
     */
    removeController(controller) {
        this.__controllers?.delete(controller);
    }
    /**
     * Fixes any properties set on the instance before upgrade time.
     * Otherwise these would shadow the accessor and break these properties.
     * The properties are stored in a Map which is played back after the
     * constructor runs.
     */
    __saveInstanceProperties() {
        const instanceProperties = new Map();
        const elementProperties = this.constructor
            .elementProperties;
        for (const p of elementProperties.keys()) {
            if (this.hasOwnProperty(p)) {
                instanceProperties.set(p, this[p]);
                delete this[p];
            }
        }
        if (instanceProperties.size > 0) {
            this.__instanceProperties = instanceProperties;
        }
    }
    /**
     * Returns the node into which the element should render and by default
     * creates and returns an open shadowRoot. Implement to customize where the
     * element's DOM is rendered. For example, to render into the element's
     * childNodes, return `this`.
     *
     * @return Returns a node into which to render.
     * @category rendering
     */
    createRenderRoot() {
        const renderRoot = this.shadowRoot ??
            this.attachShadow(this.constructor.shadowRootOptions);
        (0,_css_tag_js__WEBPACK_IMPORTED_MODULE_0__.adoptStyles)(renderRoot, this.constructor.elementStyles);
        return renderRoot;
    }
    /**
     * On first connection, creates the element's renderRoot, sets up
     * element styling, and enables updating.
     * @category lifecycle
     */
    connectedCallback() {
        // Create renderRoot before controllers `hostConnected`
        this.renderRoot ??=
            this.createRenderRoot();
        this.enableUpdating(true);
        this.__controllers?.forEach((c) => c.hostConnected?.());
    }
    /**
     * Note, this method should be considered final and not overridden. It is
     * overridden on the element instance with a function that triggers the first
     * update.
     * @category updates
     */
    enableUpdating(_requestedUpdate) { }
    /**
     * Allows for `super.disconnectedCallback()` in extensions while
     * reserving the possibility of making non-breaking feature additions
     * when disconnecting at some point in the future.
     * @category lifecycle
     */
    disconnectedCallback() {
        this.__controllers?.forEach((c) => c.hostDisconnected?.());
    }
    /**
     * Synchronizes property values when attributes change.
     *
     * Specifically, when an attribute is set, the corresponding property is set.
     * You should rarely need to implement this callback. If this method is
     * overridden, `super.attributeChangedCallback(name, _old, value)` must be
     * called.
     *
     * See [responding to attribute changes](https://developer.mozilla.org/en-US/docs/Web/API/Web_components/Using_custom_elements#responding_to_attribute_changes)
     * on MDN for more information about the `attributeChangedCallback`.
     * @category attributes
     */
    attributeChangedCallback(name, _old, value) {
        this._$attributeToProperty(name, value);
    }
    __propertyToAttribute(name, value) {
        const elemProperties = this.constructor.elementProperties;
        const options = elemProperties.get(name);
        const attr = this.constructor.__attributeNameForProperty(name, options);
        if (attr !== undefined && options.reflect === true) {
            const converter = options.converter?.toAttribute !==
                undefined
                ? options.converter
                : defaultConverter;
            const attrValue = converter.toAttribute(value, options.type);
            if (DEV_MODE &&
                this.constructor.enabledWarnings.includes('migration') &&
                attrValue === undefined) {
                issueWarning('undefined-attribute-value', `The attribute value for the ${name} property is ` +
                    `undefined on element ${this.localName}. The attribute will be ` +
                    `removed, but in the previous version of \`ReactiveElement\`, ` +
                    `the attribute would not have changed.`);
            }
            // Track if the property is being reflected to avoid
            // setting the property again via `attributeChangedCallback`. Note:
            // 1. this takes advantage of the fact that the callback is synchronous.
            // 2. will behave incorrectly if multiple attributes are in the reaction
            // stack at time of calling. However, since we process attributes
            // in `update` this should not be possible (or an extreme corner case
            // that we'd like to discover).
            // mark state reflecting
            this.__reflectingProperty = name;
            if (attrValue == null) {
                this.removeAttribute(attr);
            }
            else {
                this.setAttribute(attr, attrValue);
            }
            // mark state not reflecting
            this.__reflectingProperty = null;
        }
    }
    /** @internal */
    _$attributeToProperty(name, value) {
        const ctor = this.constructor;
        // Note, hint this as an `AttributeMap` so closure clearly understands
        // the type; it has issues with tracking types through statics
        const propName = ctor.__attributeToPropertyMap.get(name);
        // Use tracking info to avoid reflecting a property value to an attribute
        // if it was just set because the attribute changed.
        if (propName !== undefined && this.__reflectingProperty !== propName) {
            const options = ctor.getPropertyOptions(propName);
            const converter = typeof options.converter === 'function'
                ? { fromAttribute: options.converter }
                : options.converter?.fromAttribute !== undefined
                    ? options.converter
                    : defaultConverter;
            // mark state reflecting
            this.__reflectingProperty = propName;
            const convertedValue = converter.fromAttribute(value, options.type);
            this[propName] =
                convertedValue ??
                    this.__defaultValues?.get(propName) ??
                    // eslint-disable-next-line @typescript-eslint/no-explicit-any
                    convertedValue;
            // mark state not reflecting
            this.__reflectingProperty = null;
        }
    }
    /**
     * Requests an update which is processed asynchronously. This should be called
     * when an element should update based on some state not triggered by setting
     * a reactive property. In this case, pass no arguments. It should also be
     * called when manually implementing a property setter. In this case, pass the
     * property `name` and `oldValue` to ensure that any configured property
     * options are honored.
     *
     * @param name name of requesting property
     * @param oldValue old value of requesting property
     * @param options property options to use instead of the previously
     *     configured options
     * @category updates
     */
    requestUpdate(name, oldValue, options) {
        // If we have a property key, perform property update steps.
        if (name !== undefined) {
            if (DEV_MODE && name instanceof Event) {
                issueWarning(``, `The requestUpdate() method was called with an Event as the property name. This is probably a mistake caused by binding this.requestUpdate as an event listener. Instead bind a function that will call it with no arguments: () => this.requestUpdate()`);
            }
            const ctor = this.constructor;
            const newValue = this[name];
            options ??= ctor.getPropertyOptions(name);
            const changed = (options.hasChanged ?? notEqual)(newValue, oldValue) ||
                // When there is no change, check a corner case that can occur when
                // 1. there's a initial value which was not reflected
                // 2. the property is subsequently set to this value.
                // For example, `prop: {useDefault: true, reflect: true}`
                // and el.prop = 'foo'. This should be considered a change if the
                // attribute is not set because we will now reflect the property to the attribute.
                (options.useDefault &&
                    options.reflect &&
                    newValue === this.__defaultValues?.get(name) &&
                    !this.hasAttribute(ctor.__attributeNameForProperty(name, options)));
            if (changed) {
                this._$changeProperty(name, oldValue, options);
            }
            else {
                // Abort the request if the property should not be considered changed.
                return;
            }
        }
        if (this.isUpdatePending === false) {
            this.__updatePromise = this.__enqueueUpdate();
        }
    }
    /**
     * @internal
     */
    _$changeProperty(name, oldValue, { useDefault, reflect, wrapped }, initializeValue) {
        // Record default value when useDefault is used. This allows us to
        // restore this value when the attribute is removed.
        if (useDefault && !(this.__defaultValues ??= new Map()).has(name)) {
            this.__defaultValues.set(name, initializeValue ?? oldValue ?? this[name]);
            // if this is not wrapping an accessor, it must be an initial setting
            // and in this case we do not want to record the change or reflect.
            if (wrapped !== true || initializeValue !== undefined) {
                return;
            }
        }
        // TODO (justinfagnani): Create a benchmark of Map.has() + Map.set(
        // vs just Map.set()
        if (!this._$changedProperties.has(name)) {
            // On the initial change, the old value should be `undefined`, except
            // with `useDefault`
            if (!this.hasUpdated && !useDefault) {
                oldValue = undefined;
            }
            this._$changedProperties.set(name, oldValue);
        }
        // Add to reflecting properties set.
        // Note, it's important that every change has a chance to add the
        // property to `__reflectingProperties`. This ensures setting
        // attribute + property reflects correctly.
        if (reflect === true && this.__reflectingProperty !== name) {
            (this.__reflectingProperties ??= new Set()).add(name);
        }
    }
    /**
     * Sets up the element to asynchronously update.
     */
    async __enqueueUpdate() {
        this.isUpdatePending = true;
        try {
            // Ensure any previous update has resolved before updating.
            // This `await` also ensures that property changes are batched.
            await this.__updatePromise;
        }
        catch (e) {
            // Refire any previous errors async so they do not disrupt the update
            // cycle. Errors are refired so developers have a chance to observe
            // them, and this can be done by implementing
            // `window.onunhandledrejection`.
            Promise.reject(e);
        }
        const result = this.scheduleUpdate();
        // If `scheduleUpdate` returns a Promise, we await it. This is done to
        // enable coordinating updates with a scheduler. Note, the result is
        // checked to avoid delaying an additional microtask unless we need to.
        if (result != null) {
            await result;
        }
        return !this.isUpdatePending;
    }
    /**
     * Schedules an element update. You can override this method to change the
     * timing of updates by returning a Promise. The update will await the
     * returned Promise, and you should resolve the Promise to allow the update
     * to proceed. If this method is overridden, `super.scheduleUpdate()`
     * must be called.
     *
     * For instance, to schedule updates to occur just before the next frame:
     *
     * ```ts
     * override protected async scheduleUpdate(): Promise<unknown> {
     *   await new Promise((resolve) => requestAnimationFrame(() => resolve()));
     *   super.scheduleUpdate();
     * }
     * ```
     * @category updates
     */
    scheduleUpdate() {
        const result = this.performUpdate();
        if (DEV_MODE &&
            this.constructor.enabledWarnings.includes('async-perform-update') &&
            typeof result?.then ===
                'function') {
            issueWarning('async-perform-update', `Element ${this.localName} returned a Promise from performUpdate(). ` +
                `This behavior is deprecated and will be removed in a future ` +
                `version of ReactiveElement.`);
        }
        return result;
    }
    /**
     * Performs an element update. Note, if an exception is thrown during the
     * update, `firstUpdated` and `updated` will not be called.
     *
     * Call `performUpdate()` to immediately process a pending update. This should
     * generally not be needed, but it can be done in rare cases when you need to
     * update synchronously.
     *
     * @category updates
     */
    performUpdate() {
        // Abort any update if one is not pending when this is called.
        // This can happen if `performUpdate` is called early to "flush"
        // the update.
        if (!this.isUpdatePending) {
            return;
        }
        debugLogEvent?.({ kind: 'update' });
        if (!this.hasUpdated) {
            // Create renderRoot before first update. This occurs in `connectedCallback`
            // but is done here to support out of tree calls to `enableUpdating`/`performUpdate`.
            this.renderRoot ??=
                this.createRenderRoot();
            if (DEV_MODE) {
                // Produce warning if any reactive properties on the prototype are
                // shadowed by class fields. Instance fields set before upgrade are
                // deleted by this point, so any own property is caused by class field
                // initialization in the constructor.
                const ctor = this.constructor;
                const shadowedProperties = [...ctor.elementProperties.keys()].filter((p) => this.hasOwnProperty(p) && p in getPrototypeOf(this));
                if (shadowedProperties.length) {
                    throw new Error(`The following properties on element ${this.localName} will not ` +
                        `trigger updates as expected because they are set using class ` +
                        `fields: ${shadowedProperties.join(', ')}. ` +
                        `Native class fields and some compiled output will overwrite ` +
                        `accessors used for detecting changes. See ` +
                        `https://lit.dev/msg/class-field-shadowing ` +
                        `for more information.`);
                }
            }
            // Mixin instance properties once, if they exist.
            if (this.__instanceProperties) {
                // TODO (justinfagnani): should we use the stored value? Could a new value
                // have been set since we stored the own property value?
                for (const [p, value] of this.__instanceProperties) {
                    this[p] = value;
                }
                this.__instanceProperties = undefined;
            }
            // Trigger initial value reflection and populate the initial
            // `changedProperties` map, but only for the case of properties created
            // via `createProperty` on accessors, which will not have already
            // populated the `changedProperties` map since they are not set.
            // We can't know if these accessors had initializers, so we just set
            // them anyway - a difference from experimental decorators on fields and
            // standard decorators on auto-accessors.
            // For context see:
            // https://github.com/lit/lit/pull/4183#issuecomment-1711959635
            const elementProperties = this.constructor
                .elementProperties;
            if (elementProperties.size > 0) {
                for (const [p, options] of elementProperties) {
                    const { wrapped } = options;
                    const value = this[p];
                    if (wrapped === true &&
                        !this._$changedProperties.has(p) &&
                        value !== undefined) {
                        this._$changeProperty(p, undefined, options, value);
                    }
                }
            }
        }
        let shouldUpdate = false;
        const changedProperties = this._$changedProperties;
        try {
            shouldUpdate = this.shouldUpdate(changedProperties);
            if (shouldUpdate) {
                this.willUpdate(changedProperties);
                this.__controllers?.forEach((c) => c.hostUpdate?.());
                this.update(changedProperties);
            }
            else {
                this.__markUpdated();
            }
        }
        catch (e) {
            // Prevent `firstUpdated` and `updated` from running when there's an
            // update exception.
            shouldUpdate = false;
            // Ensure element can accept additional updates after an exception.
            this.__markUpdated();
            throw e;
        }
        // The update is no longer considered pending and further updates are now allowed.
        if (shouldUpdate) {
            this._$didUpdate(changedProperties);
        }
    }
    /**
     * Invoked before `update()` to compute values needed during the update.
     *
     * Implement `willUpdate` to compute property values that depend on other
     * properties and are used in the rest of the update process.
     *
     * ```ts
     * willUpdate(changedProperties) {
     *   // only need to check changed properties for an expensive computation.
     *   if (changedProperties.has('firstName') || changedProperties.has('lastName')) {
     *     this.sha = computeSHA(`${this.firstName} ${this.lastName}`);
     *   }
     * }
     *
     * render() {
     *   return html`SHA: ${this.sha}`;
     * }
     * ```
     *
     * @category updates
     */
    willUpdate(_changedProperties) { }
    // Note, this is an override point for polyfill-support.
    // @internal
    _$didUpdate(changedProperties) {
        this.__controllers?.forEach((c) => c.hostUpdated?.());
        if (!this.hasUpdated) {
            this.hasUpdated = true;
            this.firstUpdated(changedProperties);
        }
        this.updated(changedProperties);
        if (DEV_MODE &&
            this.isUpdatePending &&
            this.constructor.enabledWarnings.includes('change-in-update')) {
            issueWarning('change-in-update', `Element ${this.localName} scheduled an update ` +
                `(generally because a property was set) ` +
                `after an update completed, causing a new update to be scheduled. ` +
                `This is inefficient and should be avoided unless the next update ` +
                `can only be scheduled as a side effect of the previous update.`);
        }
    }
    __markUpdated() {
        this._$changedProperties = new Map();
        this.isUpdatePending = false;
    }
    /**
     * Returns a Promise that resolves when the element has completed updating.
     * The Promise value is a boolean that is `true` if the element completed the
     * update without triggering another update. The Promise result is `false` if
     * a property was set inside `updated()`. If the Promise is rejected, an
     * exception was thrown during the update.
     *
     * To await additional asynchronous work, override the `getUpdateComplete`
     * method. For example, it is sometimes useful to await a rendered element
     * before fulfilling this Promise. To do this, first await
     * `super.getUpdateComplete()`, then any subsequent state.
     *
     * @return A promise of a boolean that resolves to true if the update completed
     *     without triggering another update.
     * @category updates
     */
    get updateComplete() {
        return this.getUpdateComplete();
    }
    /**
     * Override point for the `updateComplete` promise.
     *
     * It is not safe to override the `updateComplete` getter directly due to a
     * limitation in TypeScript which means it is not possible to call a
     * superclass getter (e.g. `super.updateComplete.then(...)`) when the target
     * language is ES5 (https://github.com/microsoft/TypeScript/issues/338).
     * This method should be overridden instead. For example:
     *
     * ```ts
     * class MyElement extends LitElement {
     *   override async getUpdateComplete() {
     *     const result = await super.getUpdateComplete();
     *     await this._myChild.updateComplete;
     *     return result;
     *   }
     * }
     * ```
     *
     * @return A promise of a boolean that resolves to true if the update completed
     *     without triggering another update.
     * @category updates
     */
    getUpdateComplete() {
        return this.__updatePromise;
    }
    /**
     * Controls whether or not `update()` should be called when the element requests
     * an update. By default, this method always returns `true`, but this can be
     * customized to control when to update.
     *
     * @param _changedProperties Map of changed properties with old values
     * @category updates
     */
    shouldUpdate(_changedProperties) {
        return true;
    }
    /**
     * Updates the element. This method reflects property values to attributes.
     * It can be overridden to render and keep updated element DOM.
     * Setting properties inside this method will *not* trigger
     * another update.
     *
     * @param _changedProperties Map of changed properties with old values
     * @category updates
     */
    update(_changedProperties) {
        // The forEach() expression will only run when __reflectingProperties is
        // defined, and it returns undefined, setting __reflectingProperties to
        // undefined
        this.__reflectingProperties &&= this.__reflectingProperties.forEach((p) => this.__propertyToAttribute(p, this[p]));
        this.__markUpdated();
    }
    /**
     * Invoked whenever the element is updated. Implement to perform
     * post-updating tasks via DOM APIs, for example, focusing an element.
     *
     * Setting properties inside this method will trigger the element to update
     * again after this update cycle completes.
     *
     * @param _changedProperties Map of changed properties with old values
     * @category updates
     */
    updated(_changedProperties) { }
    /**
     * Invoked when the element is first updated. Implement to perform one time
     * work on the element after update.
     *
     * ```ts
     * firstUpdated() {
     *   this.renderRoot.getElementById('my-text-area').focus();
     * }
     * ```
     *
     * Setting properties inside this method will trigger the element to update
     * again after this update cycle completes.
     *
     * @param _changedProperties Map of changed properties with old values
     * @category updates
     */
    firstUpdated(_changedProperties) { }
}
/**
 * Memoized list of all element styles.
 * Created lazily on user subclasses when finalizing the class.
 * @nocollapse
 * @category styles
 */
ReactiveElement.elementStyles = [];
/**
 * Options used when calling `attachShadow`. Set this property to customize
 * the options for the shadowRoot; for example, to create a closed
 * shadowRoot: `{mode: 'closed'}`.
 *
 * Note, these options are used in `createRenderRoot`. If this method
 * is customized, options should be respected if possible.
 * @nocollapse
 * @category rendering
 */
ReactiveElement.shadowRootOptions = { mode: 'open' };
// Assigned here to work around a jscompiler bug with static fields
// when compiling to ES5.
// https://github.com/google/closure-compiler/issues/3177
ReactiveElement[JSCompiler_renameProperty('elementProperties', ReactiveElement)] = new Map();
ReactiveElement[JSCompiler_renameProperty('finalized', ReactiveElement)] = new Map();
// Apply polyfills if available
polyfillSupport?.({ ReactiveElement });
// Dev mode warnings...
if (DEV_MODE) {
    // Default warning set.
    ReactiveElement.enabledWarnings = [
        'change-in-update',
        'async-perform-update',
    ];
    const ensureOwnWarnings = function (ctor) {
        if (!ctor.hasOwnProperty(JSCompiler_renameProperty('enabledWarnings', ctor))) {
            ctor.enabledWarnings = ctor.enabledWarnings.slice();
        }
    };
    ReactiveElement.enableWarning = function (warning) {
        ensureOwnWarnings(this);
        if (!this.enabledWarnings.includes(warning)) {
            this.enabledWarnings.push(warning);
        }
    };
    ReactiveElement.disableWarning = function (warning) {
        ensureOwnWarnings(this);
        const i = this.enabledWarnings.indexOf(warning);
        if (i >= 0) {
            this.enabledWarnings.splice(i, 1);
        }
    };
}
// IMPORTANT: do not change the property name or the assignment expression.
// This line will be used in regexes to search for ReactiveElement usage.
(global.reactiveElementVersions ??= []).push('2.1.1');
if (DEV_MODE && global.reactiveElementVersions.length > 1) {
    queueMicrotask(() => {
        issueWarning('multiple-versions', `Multiple versions of Lit loaded. Loading multiple versions ` +
            `is not recommended.`);
    });
}
//# sourceMappingURL=reactive-element.js.map

}),
"./node_modules/.pnpm/js-yaml@4.1.0/node_modules/js-yaml/dist/js-yaml.mjs": 
/*!********************************************************************************!*\
  !*** ./node_modules/.pnpm/js-yaml@4.1.0/node_modules/js-yaml/dist/js-yaml.mjs ***!
  \********************************************************************************/
(function (__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  CORE_SCHEMA: () => (CORE_SCHEMA),
  DEFAULT_SCHEMA: () => (DEFAULT_SCHEMA),
  FAILSAFE_SCHEMA: () => (FAILSAFE_SCHEMA),
  JSON_SCHEMA: () => (JSON_SCHEMA),
  Schema: () => (Schema),
  Type: () => (Type),
  YAMLException: () => (YAMLException),
  "default": () => (__WEBPACK_DEFAULT_EXPORT__),
  dump: () => (dump),
  load: () => (load),
  loadAll: () => (loadAll),
  safeDump: () => (safeDump),
  safeLoad: () => (safeLoad),
  safeLoadAll: () => (safeLoadAll),
  types: () => (types)
});

/*! js-yaml 4.1.0 https://github.com/nodeca/js-yaml @license MIT */
function isNothing(subject) {
  return (typeof subject === 'undefined') || (subject === null);
}


function isObject(subject) {
  return (typeof subject === 'object') && (subject !== null);
}


function toArray(sequence) {
  if (Array.isArray(sequence)) return sequence;
  else if (isNothing(sequence)) return [];

  return [ sequence ];
}


function extend(target, source) {
  var index, length, key, sourceKeys;

  if (source) {
    sourceKeys = Object.keys(source);

    for (index = 0, length = sourceKeys.length; index < length; index += 1) {
      key = sourceKeys[index];
      target[key] = source[key];
    }
  }

  return target;
}


function repeat(string, count) {
  var result = '', cycle;

  for (cycle = 0; cycle < count; cycle += 1) {
    result += string;
  }

  return result;
}


function isNegativeZero(number) {
  return (number === 0) && (Number.NEGATIVE_INFINITY === 1 / number);
}


var isNothing_1      = isNothing;
var isObject_1       = isObject;
var toArray_1        = toArray;
var repeat_1         = repeat;
var isNegativeZero_1 = isNegativeZero;
var extend_1         = extend;

var common = {
	isNothing: isNothing_1,
	isObject: isObject_1,
	toArray: toArray_1,
	repeat: repeat_1,
	isNegativeZero: isNegativeZero_1,
	extend: extend_1
};

// YAML error class. http://stackoverflow.com/questions/8458984


function formatError(exception, compact) {
  var where = '', message = exception.reason || '(unknown reason)';

  if (!exception.mark) return message;

  if (exception.mark.name) {
    where += 'in "' + exception.mark.name + '" ';
  }

  where += '(' + (exception.mark.line + 1) + ':' + (exception.mark.column + 1) + ')';

  if (!compact && exception.mark.snippet) {
    where += '\n\n' + exception.mark.snippet;
  }

  return message + ' ' + where;
}


function YAMLException$1(reason, mark) {
  // Super constructor
  Error.call(this);

  this.name = 'YAMLException';
  this.reason = reason;
  this.mark = mark;
  this.message = formatError(this, false);

  // Include stack trace in error object
  if (Error.captureStackTrace) {
    // Chrome and NodeJS
    Error.captureStackTrace(this, this.constructor);
  } else {
    // FF, IE 10+ and Safari 6+. Fallback for others
    this.stack = (new Error()).stack || '';
  }
}


// Inherit from Error
YAMLException$1.prototype = Object.create(Error.prototype);
YAMLException$1.prototype.constructor = YAMLException$1;


YAMLException$1.prototype.toString = function toString(compact) {
  return this.name + ': ' + formatError(this, compact);
};


var exception = YAMLException$1;

// get snippet for a single line, respecting maxLength
function getLine(buffer, lineStart, lineEnd, position, maxLineLength) {
  var head = '';
  var tail = '';
  var maxHalfLength = Math.floor(maxLineLength / 2) - 1;

  if (position - lineStart > maxHalfLength) {
    head = ' ... ';
    lineStart = position - maxHalfLength + head.length;
  }

  if (lineEnd - position > maxHalfLength) {
    tail = ' ...';
    lineEnd = position + maxHalfLength - tail.length;
  }

  return {
    str: head + buffer.slice(lineStart, lineEnd).replace(/\t/g, '→') + tail,
    pos: position - lineStart + head.length // relative position
  };
}


function padStart(string, max) {
  return common.repeat(' ', max - string.length) + string;
}


function makeSnippet(mark, options) {
  options = Object.create(options || null);

  if (!mark.buffer) return null;

  if (!options.maxLength) options.maxLength = 79;
  if (typeof options.indent      !== 'number') options.indent      = 1;
  if (typeof options.linesBefore !== 'number') options.linesBefore = 3;
  if (typeof options.linesAfter  !== 'number') options.linesAfter  = 2;

  var re = /\r?\n|\r|\0/g;
  var lineStarts = [ 0 ];
  var lineEnds = [];
  var match;
  var foundLineNo = -1;

  while ((match = re.exec(mark.buffer))) {
    lineEnds.push(match.index);
    lineStarts.push(match.index + match[0].length);

    if (mark.position <= match.index && foundLineNo < 0) {
      foundLineNo = lineStarts.length - 2;
    }
  }

  if (foundLineNo < 0) foundLineNo = lineStarts.length - 1;

  var result = '', i, line;
  var lineNoLength = Math.min(mark.line + options.linesAfter, lineEnds.length).toString().length;
  var maxLineLength = options.maxLength - (options.indent + lineNoLength + 3);

  for (i = 1; i <= options.linesBefore; i++) {
    if (foundLineNo - i < 0) break;
    line = getLine(
      mark.buffer,
      lineStarts[foundLineNo - i],
      lineEnds[foundLineNo - i],
      mark.position - (lineStarts[foundLineNo] - lineStarts[foundLineNo - i]),
      maxLineLength
    );
    result = common.repeat(' ', options.indent) + padStart((mark.line - i + 1).toString(), lineNoLength) +
      ' | ' + line.str + '\n' + result;
  }

  line = getLine(mark.buffer, lineStarts[foundLineNo], lineEnds[foundLineNo], mark.position, maxLineLength);
  result += common.repeat(' ', options.indent) + padStart((mark.line + 1).toString(), lineNoLength) +
    ' | ' + line.str + '\n';
  result += common.repeat('-', options.indent + lineNoLength + 3 + line.pos) + '^' + '\n';

  for (i = 1; i <= options.linesAfter; i++) {
    if (foundLineNo + i >= lineEnds.length) break;
    line = getLine(
      mark.buffer,
      lineStarts[foundLineNo + i],
      lineEnds[foundLineNo + i],
      mark.position - (lineStarts[foundLineNo] - lineStarts[foundLineNo + i]),
      maxLineLength
    );
    result += common.repeat(' ', options.indent) + padStart((mark.line + i + 1).toString(), lineNoLength) +
      ' | ' + line.str + '\n';
  }

  return result.replace(/\n$/, '');
}


var snippet = makeSnippet;

var TYPE_CONSTRUCTOR_OPTIONS = [
  'kind',
  'multi',
  'resolve',
  'construct',
  'instanceOf',
  'predicate',
  'represent',
  'representName',
  'defaultStyle',
  'styleAliases'
];

var YAML_NODE_KINDS = [
  'scalar',
  'sequence',
  'mapping'
];

function compileStyleAliases(map) {
  var result = {};

  if (map !== null) {
    Object.keys(map).forEach(function (style) {
      map[style].forEach(function (alias) {
        result[String(alias)] = style;
      });
    });
  }

  return result;
}

function Type$1(tag, options) {
  options = options || {};

  Object.keys(options).forEach(function (name) {
    if (TYPE_CONSTRUCTOR_OPTIONS.indexOf(name) === -1) {
      throw new exception('Unknown option "' + name + '" is met in definition of "' + tag + '" YAML type.');
    }
  });

  // TODO: Add tag format check.
  this.options       = options; // keep original options in case user wants to extend this type later
  this.tag           = tag;
  this.kind          = options['kind']          || null;
  this.resolve       = options['resolve']       || function () { return true; };
  this.construct     = options['construct']     || function (data) { return data; };
  this.instanceOf    = options['instanceOf']    || null;
  this.predicate     = options['predicate']     || null;
  this.represent     = options['represent']     || null;
  this.representName = options['representName'] || null;
  this.defaultStyle  = options['defaultStyle']  || null;
  this.multi         = options['multi']         || false;
  this.styleAliases  = compileStyleAliases(options['styleAliases'] || null);

  if (YAML_NODE_KINDS.indexOf(this.kind) === -1) {
    throw new exception('Unknown kind "' + this.kind + '" is specified for "' + tag + '" YAML type.');
  }
}

var type = Type$1;

/*eslint-disable max-len*/





function compileList(schema, name) {
  var result = [];

  schema[name].forEach(function (currentType) {
    var newIndex = result.length;

    result.forEach(function (previousType, previousIndex) {
      if (previousType.tag === currentType.tag &&
          previousType.kind === currentType.kind &&
          previousType.multi === currentType.multi) {

        newIndex = previousIndex;
      }
    });

    result[newIndex] = currentType;
  });

  return result;
}


function compileMap(/* lists... */) {
  var result = {
        scalar: {},
        sequence: {},
        mapping: {},
        fallback: {},
        multi: {
          scalar: [],
          sequence: [],
          mapping: [],
          fallback: []
        }
      }, index, length;

  function collectType(type) {
    if (type.multi) {
      result.multi[type.kind].push(type);
      result.multi['fallback'].push(type);
    } else {
      result[type.kind][type.tag] = result['fallback'][type.tag] = type;
    }
  }

  for (index = 0, length = arguments.length; index < length; index += 1) {
    arguments[index].forEach(collectType);
  }
  return result;
}


function Schema$1(definition) {
  return this.extend(definition);
}


Schema$1.prototype.extend = function extend(definition) {
  var implicit = [];
  var explicit = [];

  if (definition instanceof type) {
    // Schema.extend(type)
    explicit.push(definition);

  } else if (Array.isArray(definition)) {
    // Schema.extend([ type1, type2, ... ])
    explicit = explicit.concat(definition);

  } else if (definition && (Array.isArray(definition.implicit) || Array.isArray(definition.explicit))) {
    // Schema.extend({ explicit: [ type1, type2, ... ], implicit: [ type1, type2, ... ] })
    if (definition.implicit) implicit = implicit.concat(definition.implicit);
    if (definition.explicit) explicit = explicit.concat(definition.explicit);

  } else {
    throw new exception('Schema.extend argument should be a Type, [ Type ], ' +
      'or a schema definition ({ implicit: [...], explicit: [...] })');
  }

  implicit.forEach(function (type$1) {
    if (!(type$1 instanceof type)) {
      throw new exception('Specified list of YAML types (or a single Type object) contains a non-Type object.');
    }

    if (type$1.loadKind && type$1.loadKind !== 'scalar') {
      throw new exception('There is a non-scalar type in the implicit list of a schema. Implicit resolving of such types is not supported.');
    }

    if (type$1.multi) {
      throw new exception('There is a multi type in the implicit list of a schema. Multi tags can only be listed as explicit.');
    }
  });

  explicit.forEach(function (type$1) {
    if (!(type$1 instanceof type)) {
      throw new exception('Specified list of YAML types (or a single Type object) contains a non-Type object.');
    }
  });

  var result = Object.create(Schema$1.prototype);

  result.implicit = (this.implicit || []).concat(implicit);
  result.explicit = (this.explicit || []).concat(explicit);

  result.compiledImplicit = compileList(result, 'implicit');
  result.compiledExplicit = compileList(result, 'explicit');
  result.compiledTypeMap  = compileMap(result.compiledImplicit, result.compiledExplicit);

  return result;
};


var schema = Schema$1;

var str = new type('tag:yaml.org,2002:str', {
  kind: 'scalar',
  construct: function (data) { return data !== null ? data : ''; }
});

var seq = new type('tag:yaml.org,2002:seq', {
  kind: 'sequence',
  construct: function (data) { return data !== null ? data : []; }
});

var map = new type('tag:yaml.org,2002:map', {
  kind: 'mapping',
  construct: function (data) { return data !== null ? data : {}; }
});

var failsafe = new schema({
  explicit: [
    str,
    seq,
    map
  ]
});

function resolveYamlNull(data) {
  if (data === null) return true;

  var max = data.length;

  return (max === 1 && data === '~') ||
         (max === 4 && (data === 'null' || data === 'Null' || data === 'NULL'));
}

function constructYamlNull() {
  return null;
}

function isNull(object) {
  return object === null;
}

var _null = new type('tag:yaml.org,2002:null', {
  kind: 'scalar',
  resolve: resolveYamlNull,
  construct: constructYamlNull,
  predicate: isNull,
  represent: {
    canonical: function () { return '~';    },
    lowercase: function () { return 'null'; },
    uppercase: function () { return 'NULL'; },
    camelcase: function () { return 'Null'; },
    empty:     function () { return '';     }
  },
  defaultStyle: 'lowercase'
});

function resolveYamlBoolean(data) {
  if (data === null) return false;

  var max = data.length;

  return (max === 4 && (data === 'true' || data === 'True' || data === 'TRUE')) ||
         (max === 5 && (data === 'false' || data === 'False' || data === 'FALSE'));
}

function constructYamlBoolean(data) {
  return data === 'true' ||
         data === 'True' ||
         data === 'TRUE';
}

function isBoolean(object) {
  return Object.prototype.toString.call(object) === '[object Boolean]';
}

var bool = new type('tag:yaml.org,2002:bool', {
  kind: 'scalar',
  resolve: resolveYamlBoolean,
  construct: constructYamlBoolean,
  predicate: isBoolean,
  represent: {
    lowercase: function (object) { return object ? 'true' : 'false'; },
    uppercase: function (object) { return object ? 'TRUE' : 'FALSE'; },
    camelcase: function (object) { return object ? 'True' : 'False'; }
  },
  defaultStyle: 'lowercase'
});

function isHexCode(c) {
  return ((0x30/* 0 */ <= c) && (c <= 0x39/* 9 */)) ||
         ((0x41/* A */ <= c) && (c <= 0x46/* F */)) ||
         ((0x61/* a */ <= c) && (c <= 0x66/* f */));
}

function isOctCode(c) {
  return ((0x30/* 0 */ <= c) && (c <= 0x37/* 7 */));
}

function isDecCode(c) {
  return ((0x30/* 0 */ <= c) && (c <= 0x39/* 9 */));
}

function resolveYamlInteger(data) {
  if (data === null) return false;

  var max = data.length,
      index = 0,
      hasDigits = false,
      ch;

  if (!max) return false;

  ch = data[index];

  // sign
  if (ch === '-' || ch === '+') {
    ch = data[++index];
  }

  if (ch === '0') {
    // 0
    if (index + 1 === max) return true;
    ch = data[++index];

    // base 2, base 8, base 16

    if (ch === 'b') {
      // base 2
      index++;

      for (; index < max; index++) {
        ch = data[index];
        if (ch === '_') continue;
        if (ch !== '0' && ch !== '1') return false;
        hasDigits = true;
      }
      return hasDigits && ch !== '_';
    }


    if (ch === 'x') {
      // base 16
      index++;

      for (; index < max; index++) {
        ch = data[index];
        if (ch === '_') continue;
        if (!isHexCode(data.charCodeAt(index))) return false;
        hasDigits = true;
      }
      return hasDigits && ch !== '_';
    }


    if (ch === 'o') {
      // base 8
      index++;

      for (; index < max; index++) {
        ch = data[index];
        if (ch === '_') continue;
        if (!isOctCode(data.charCodeAt(index))) return false;
        hasDigits = true;
      }
      return hasDigits && ch !== '_';
    }
  }

  // base 10 (except 0)

  // value should not start with `_`;
  if (ch === '_') return false;

  for (; index < max; index++) {
    ch = data[index];
    if (ch === '_') continue;
    if (!isDecCode(data.charCodeAt(index))) {
      return false;
    }
    hasDigits = true;
  }

  // Should have digits and should not end with `_`
  if (!hasDigits || ch === '_') return false;

  return true;
}

function constructYamlInteger(data) {
  var value = data, sign = 1, ch;

  if (value.indexOf('_') !== -1) {
    value = value.replace(/_/g, '');
  }

  ch = value[0];

  if (ch === '-' || ch === '+') {
    if (ch === '-') sign = -1;
    value = value.slice(1);
    ch = value[0];
  }

  if (value === '0') return 0;

  if (ch === '0') {
    if (value[1] === 'b') return sign * parseInt(value.slice(2), 2);
    if (value[1] === 'x') return sign * parseInt(value.slice(2), 16);
    if (value[1] === 'o') return sign * parseInt(value.slice(2), 8);
  }

  return sign * parseInt(value, 10);
}

function isInteger(object) {
  return (Object.prototype.toString.call(object)) === '[object Number]' &&
         (object % 1 === 0 && !common.isNegativeZero(object));
}

var int = new type('tag:yaml.org,2002:int', {
  kind: 'scalar',
  resolve: resolveYamlInteger,
  construct: constructYamlInteger,
  predicate: isInteger,
  represent: {
    binary:      function (obj) { return obj >= 0 ? '0b' + obj.toString(2) : '-0b' + obj.toString(2).slice(1); },
    octal:       function (obj) { return obj >= 0 ? '0o'  + obj.toString(8) : '-0o'  + obj.toString(8).slice(1); },
    decimal:     function (obj) { return obj.toString(10); },
    /* eslint-disable max-len */
    hexadecimal: function (obj) { return obj >= 0 ? '0x' + obj.toString(16).toUpperCase() :  '-0x' + obj.toString(16).toUpperCase().slice(1); }
  },
  defaultStyle: 'decimal',
  styleAliases: {
    binary:      [ 2,  'bin' ],
    octal:       [ 8,  'oct' ],
    decimal:     [ 10, 'dec' ],
    hexadecimal: [ 16, 'hex' ]
  }
});

var YAML_FLOAT_PATTERN = new RegExp(
  // 2.5e4, 2.5 and integers
  '^(?:[-+]?(?:[0-9][0-9_]*)(?:\\.[0-9_]*)?(?:[eE][-+]?[0-9]+)?' +
  // .2e4, .2
  // special case, seems not from spec
  '|\\.[0-9_]+(?:[eE][-+]?[0-9]+)?' +
  // .inf
  '|[-+]?\\.(?:inf|Inf|INF)' +
  // .nan
  '|\\.(?:nan|NaN|NAN))$');

function resolveYamlFloat(data) {
  if (data === null) return false;

  if (!YAML_FLOAT_PATTERN.test(data) ||
      // Quick hack to not allow integers end with `_`
      // Probably should update regexp & check speed
      data[data.length - 1] === '_') {
    return false;
  }

  return true;
}

function constructYamlFloat(data) {
  var value, sign;

  value  = data.replace(/_/g, '').toLowerCase();
  sign   = value[0] === '-' ? -1 : 1;

  if ('+-'.indexOf(value[0]) >= 0) {
    value = value.slice(1);
  }

  if (value === '.inf') {
    return (sign === 1) ? Number.POSITIVE_INFINITY : Number.NEGATIVE_INFINITY;

  } else if (value === '.nan') {
    return NaN;
  }
  return sign * parseFloat(value, 10);
}


var SCIENTIFIC_WITHOUT_DOT = /^[-+]?[0-9]+e/;

function representYamlFloat(object, style) {
  var res;

  if (isNaN(object)) {
    switch (style) {
      case 'lowercase': return '.nan';
      case 'uppercase': return '.NAN';
      case 'camelcase': return '.NaN';
    }
  } else if (Number.POSITIVE_INFINITY === object) {
    switch (style) {
      case 'lowercase': return '.inf';
      case 'uppercase': return '.INF';
      case 'camelcase': return '.Inf';
    }
  } else if (Number.NEGATIVE_INFINITY === object) {
    switch (style) {
      case 'lowercase': return '-.inf';
      case 'uppercase': return '-.INF';
      case 'camelcase': return '-.Inf';
    }
  } else if (common.isNegativeZero(object)) {
    return '-0.0';
  }

  res = object.toString(10);

  // JS stringifier can build scientific format without dots: 5e-100,
  // while YAML requres dot: 5.e-100. Fix it with simple hack

  return SCIENTIFIC_WITHOUT_DOT.test(res) ? res.replace('e', '.e') : res;
}

function isFloat(object) {
  return (Object.prototype.toString.call(object) === '[object Number]') &&
         (object % 1 !== 0 || common.isNegativeZero(object));
}

var float = new type('tag:yaml.org,2002:float', {
  kind: 'scalar',
  resolve: resolveYamlFloat,
  construct: constructYamlFloat,
  predicate: isFloat,
  represent: representYamlFloat,
  defaultStyle: 'lowercase'
});

var json = failsafe.extend({
  implicit: [
    _null,
    bool,
    int,
    float
  ]
});

var core = json;

var YAML_DATE_REGEXP = new RegExp(
  '^([0-9][0-9][0-9][0-9])'          + // [1] year
  '-([0-9][0-9])'                    + // [2] month
  '-([0-9][0-9])$');                   // [3] day

var YAML_TIMESTAMP_REGEXP = new RegExp(
  '^([0-9][0-9][0-9][0-9])'          + // [1] year
  '-([0-9][0-9]?)'                   + // [2] month
  '-([0-9][0-9]?)'                   + // [3] day
  '(?:[Tt]|[ \\t]+)'                 + // ...
  '([0-9][0-9]?)'                    + // [4] hour
  ':([0-9][0-9])'                    + // [5] minute
  ':([0-9][0-9])'                    + // [6] second
  '(?:\\.([0-9]*))?'                 + // [7] fraction
  '(?:[ \\t]*(Z|([-+])([0-9][0-9]?)' + // [8] tz [9] tz_sign [10] tz_hour
  '(?::([0-9][0-9]))?))?$');           // [11] tz_minute

function resolveYamlTimestamp(data) {
  if (data === null) return false;
  if (YAML_DATE_REGEXP.exec(data) !== null) return true;
  if (YAML_TIMESTAMP_REGEXP.exec(data) !== null) return true;
  return false;
}

function constructYamlTimestamp(data) {
  var match, year, month, day, hour, minute, second, fraction = 0,
      delta = null, tz_hour, tz_minute, date;

  match = YAML_DATE_REGEXP.exec(data);
  if (match === null) match = YAML_TIMESTAMP_REGEXP.exec(data);

  if (match === null) throw new Error('Date resolve error');

  // match: [1] year [2] month [3] day

  year = +(match[1]);
  month = +(match[2]) - 1; // JS month starts with 0
  day = +(match[3]);

  if (!match[4]) { // no hour
    return new Date(Date.UTC(year, month, day));
  }

  // match: [4] hour [5] minute [6] second [7] fraction

  hour = +(match[4]);
  minute = +(match[5]);
  second = +(match[6]);

  if (match[7]) {
    fraction = match[7].slice(0, 3);
    while (fraction.length < 3) { // milli-seconds
      fraction += '0';
    }
    fraction = +fraction;
  }

  // match: [8] tz [9] tz_sign [10] tz_hour [11] tz_minute

  if (match[9]) {
    tz_hour = +(match[10]);
    tz_minute = +(match[11] || 0);
    delta = (tz_hour * 60 + tz_minute) * 60000; // delta in mili-seconds
    if (match[9] === '-') delta = -delta;
  }

  date = new Date(Date.UTC(year, month, day, hour, minute, second, fraction));

  if (delta) date.setTime(date.getTime() - delta);

  return date;
}

function representYamlTimestamp(object /*, style*/) {
  return object.toISOString();
}

var timestamp = new type('tag:yaml.org,2002:timestamp', {
  kind: 'scalar',
  resolve: resolveYamlTimestamp,
  construct: constructYamlTimestamp,
  instanceOf: Date,
  represent: representYamlTimestamp
});

function resolveYamlMerge(data) {
  return data === '<<' || data === null;
}

var merge = new type('tag:yaml.org,2002:merge', {
  kind: 'scalar',
  resolve: resolveYamlMerge
});

/*eslint-disable no-bitwise*/





// [ 64, 65, 66 ] -> [ padding, CR, LF ]
var BASE64_MAP = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=\n\r';


function resolveYamlBinary(data) {
  if (data === null) return false;

  var code, idx, bitlen = 0, max = data.length, map = BASE64_MAP;

  // Convert one by one.
  for (idx = 0; idx < max; idx++) {
    code = map.indexOf(data.charAt(idx));

    // Skip CR/LF
    if (code > 64) continue;

    // Fail on illegal characters
    if (code < 0) return false;

    bitlen += 6;
  }

  // If there are any bits left, source was corrupted
  return (bitlen % 8) === 0;
}

function constructYamlBinary(data) {
  var idx, tailbits,
      input = data.replace(/[\r\n=]/g, ''), // remove CR/LF & padding to simplify scan
      max = input.length,
      map = BASE64_MAP,
      bits = 0,
      result = [];

  // Collect by 6*4 bits (3 bytes)

  for (idx = 0; idx < max; idx++) {
    if ((idx % 4 === 0) && idx) {
      result.push((bits >> 16) & 0xFF);
      result.push((bits >> 8) & 0xFF);
      result.push(bits & 0xFF);
    }

    bits = (bits << 6) | map.indexOf(input.charAt(idx));
  }

  // Dump tail

  tailbits = (max % 4) * 6;

  if (tailbits === 0) {
    result.push((bits >> 16) & 0xFF);
    result.push((bits >> 8) & 0xFF);
    result.push(bits & 0xFF);
  } else if (tailbits === 18) {
    result.push((bits >> 10) & 0xFF);
    result.push((bits >> 2) & 0xFF);
  } else if (tailbits === 12) {
    result.push((bits >> 4) & 0xFF);
  }

  return new Uint8Array(result);
}

function representYamlBinary(object /*, style*/) {
  var result = '', bits = 0, idx, tail,
      max = object.length,
      map = BASE64_MAP;

  // Convert every three bytes to 4 ASCII characters.

  for (idx = 0; idx < max; idx++) {
    if ((idx % 3 === 0) && idx) {
      result += map[(bits >> 18) & 0x3F];
      result += map[(bits >> 12) & 0x3F];
      result += map[(bits >> 6) & 0x3F];
      result += map[bits & 0x3F];
    }

    bits = (bits << 8) + object[idx];
  }

  // Dump tail

  tail = max % 3;

  if (tail === 0) {
    result += map[(bits >> 18) & 0x3F];
    result += map[(bits >> 12) & 0x3F];
    result += map[(bits >> 6) & 0x3F];
    result += map[bits & 0x3F];
  } else if (tail === 2) {
    result += map[(bits >> 10) & 0x3F];
    result += map[(bits >> 4) & 0x3F];
    result += map[(bits << 2) & 0x3F];
    result += map[64];
  } else if (tail === 1) {
    result += map[(bits >> 2) & 0x3F];
    result += map[(bits << 4) & 0x3F];
    result += map[64];
    result += map[64];
  }

  return result;
}

function isBinary(obj) {
  return Object.prototype.toString.call(obj) ===  '[object Uint8Array]';
}

var binary = new type('tag:yaml.org,2002:binary', {
  kind: 'scalar',
  resolve: resolveYamlBinary,
  construct: constructYamlBinary,
  predicate: isBinary,
  represent: representYamlBinary
});

var _hasOwnProperty$3 = Object.prototype.hasOwnProperty;
var _toString$2       = Object.prototype.toString;

function resolveYamlOmap(data) {
  if (data === null) return true;

  var objectKeys = [], index, length, pair, pairKey, pairHasKey,
      object = data;

  for (index = 0, length = object.length; index < length; index += 1) {
    pair = object[index];
    pairHasKey = false;

    if (_toString$2.call(pair) !== '[object Object]') return false;

    for (pairKey in pair) {
      if (_hasOwnProperty$3.call(pair, pairKey)) {
        if (!pairHasKey) pairHasKey = true;
        else return false;
      }
    }

    if (!pairHasKey) return false;

    if (objectKeys.indexOf(pairKey) === -1) objectKeys.push(pairKey);
    else return false;
  }

  return true;
}

function constructYamlOmap(data) {
  return data !== null ? data : [];
}

var omap = new type('tag:yaml.org,2002:omap', {
  kind: 'sequence',
  resolve: resolveYamlOmap,
  construct: constructYamlOmap
});

var _toString$1 = Object.prototype.toString;

function resolveYamlPairs(data) {
  if (data === null) return true;

  var index, length, pair, keys, result,
      object = data;

  result = new Array(object.length);

  for (index = 0, length = object.length; index < length; index += 1) {
    pair = object[index];

    if (_toString$1.call(pair) !== '[object Object]') return false;

    keys = Object.keys(pair);

    if (keys.length !== 1) return false;

    result[index] = [ keys[0], pair[keys[0]] ];
  }

  return true;
}

function constructYamlPairs(data) {
  if (data === null) return [];

  var index, length, pair, keys, result,
      object = data;

  result = new Array(object.length);

  for (index = 0, length = object.length; index < length; index += 1) {
    pair = object[index];

    keys = Object.keys(pair);

    result[index] = [ keys[0], pair[keys[0]] ];
  }

  return result;
}

var pairs = new type('tag:yaml.org,2002:pairs', {
  kind: 'sequence',
  resolve: resolveYamlPairs,
  construct: constructYamlPairs
});

var _hasOwnProperty$2 = Object.prototype.hasOwnProperty;

function resolveYamlSet(data) {
  if (data === null) return true;

  var key, object = data;

  for (key in object) {
    if (_hasOwnProperty$2.call(object, key)) {
      if (object[key] !== null) return false;
    }
  }

  return true;
}

function constructYamlSet(data) {
  return data !== null ? data : {};
}

var set = new type('tag:yaml.org,2002:set', {
  kind: 'mapping',
  resolve: resolveYamlSet,
  construct: constructYamlSet
});

var _default = core.extend({
  implicit: [
    timestamp,
    merge
  ],
  explicit: [
    binary,
    omap,
    pairs,
    set
  ]
});

/*eslint-disable max-len,no-use-before-define*/







var _hasOwnProperty$1 = Object.prototype.hasOwnProperty;


var CONTEXT_FLOW_IN   = 1;
var CONTEXT_FLOW_OUT  = 2;
var CONTEXT_BLOCK_IN  = 3;
var CONTEXT_BLOCK_OUT = 4;


var CHOMPING_CLIP  = 1;
var CHOMPING_STRIP = 2;
var CHOMPING_KEEP  = 3;


var PATTERN_NON_PRINTABLE         = /[\x00-\x08\x0B\x0C\x0E-\x1F\x7F-\x84\x86-\x9F\uFFFE\uFFFF]|[\uD800-\uDBFF](?![\uDC00-\uDFFF])|(?:[^\uD800-\uDBFF]|^)[\uDC00-\uDFFF]/;
var PATTERN_NON_ASCII_LINE_BREAKS = /[\x85\u2028\u2029]/;
var PATTERN_FLOW_INDICATORS       = /[,\[\]\{\}]/;
var PATTERN_TAG_HANDLE            = /^(?:!|!!|![a-z\-]+!)$/i;
var PATTERN_TAG_URI               = /^(?:!|[^,\[\]\{\}])(?:%[0-9a-f]{2}|[0-9a-z\-#;\/\?:@&=\+\$,_\.!~\*'\(\)\[\]])*$/i;


function _class(obj) { return Object.prototype.toString.call(obj); }

function is_EOL(c) {
  return (c === 0x0A/* LF */) || (c === 0x0D/* CR */);
}

function is_WHITE_SPACE(c) {
  return (c === 0x09/* Tab */) || (c === 0x20/* Space */);
}

function is_WS_OR_EOL(c) {
  return (c === 0x09/* Tab */) ||
         (c === 0x20/* Space */) ||
         (c === 0x0A/* LF */) ||
         (c === 0x0D/* CR */);
}

function is_FLOW_INDICATOR(c) {
  return c === 0x2C/* , */ ||
         c === 0x5B/* [ */ ||
         c === 0x5D/* ] */ ||
         c === 0x7B/* { */ ||
         c === 0x7D/* } */;
}

function fromHexCode(c) {
  var lc;

  if ((0x30/* 0 */ <= c) && (c <= 0x39/* 9 */)) {
    return c - 0x30;
  }

  /*eslint-disable no-bitwise*/
  lc = c | 0x20;

  if ((0x61/* a */ <= lc) && (lc <= 0x66/* f */)) {
    return lc - 0x61 + 10;
  }

  return -1;
}

function escapedHexLen(c) {
  if (c === 0x78/* x */) { return 2; }
  if (c === 0x75/* u */) { return 4; }
  if (c === 0x55/* U */) { return 8; }
  return 0;
}

function fromDecimalCode(c) {
  if ((0x30/* 0 */ <= c) && (c <= 0x39/* 9 */)) {
    return c - 0x30;
  }

  return -1;
}

function simpleEscapeSequence(c) {
  /* eslint-disable indent */
  return (c === 0x30/* 0 */) ? '\x00' :
        (c === 0x61/* a */) ? '\x07' :
        (c === 0x62/* b */) ? '\x08' :
        (c === 0x74/* t */) ? '\x09' :
        (c === 0x09/* Tab */) ? '\x09' :
        (c === 0x6E/* n */) ? '\x0A' :
        (c === 0x76/* v */) ? '\x0B' :
        (c === 0x66/* f */) ? '\x0C' :
        (c === 0x72/* r */) ? '\x0D' :
        (c === 0x65/* e */) ? '\x1B' :
        (c === 0x20/* Space */) ? ' ' :
        (c === 0x22/* " */) ? '\x22' :
        (c === 0x2F/* / */) ? '/' :
        (c === 0x5C/* \ */) ? '\x5C' :
        (c === 0x4E/* N */) ? '\x85' :
        (c === 0x5F/* _ */) ? '\xA0' :
        (c === 0x4C/* L */) ? '\u2028' :
        (c === 0x50/* P */) ? '\u2029' : '';
}

function charFromCodepoint(c) {
  if (c <= 0xFFFF) {
    return String.fromCharCode(c);
  }
  // Encode UTF-16 surrogate pair
  // https://en.wikipedia.org/wiki/UTF-16#Code_points_U.2B010000_to_U.2B10FFFF
  return String.fromCharCode(
    ((c - 0x010000) >> 10) + 0xD800,
    ((c - 0x010000) & 0x03FF) + 0xDC00
  );
}

var simpleEscapeCheck = new Array(256); // integer, for fast access
var simpleEscapeMap = new Array(256);
for (var i = 0; i < 256; i++) {
  simpleEscapeCheck[i] = simpleEscapeSequence(i) ? 1 : 0;
  simpleEscapeMap[i] = simpleEscapeSequence(i);
}


function State$1(input, options) {
  this.input = input;

  this.filename  = options['filename']  || null;
  this.schema    = options['schema']    || _default;
  this.onWarning = options['onWarning'] || null;
  // (Hidden) Remove? makes the loader to expect YAML 1.1 documents
  // if such documents have no explicit %YAML directive
  this.legacy    = options['legacy']    || false;

  this.json      = options['json']      || false;
  this.listener  = options['listener']  || null;

  this.implicitTypes = this.schema.compiledImplicit;
  this.typeMap       = this.schema.compiledTypeMap;

  this.length     = input.length;
  this.position   = 0;
  this.line       = 0;
  this.lineStart  = 0;
  this.lineIndent = 0;

  // position of first leading tab in the current line,
  // used to make sure there are no tabs in the indentation
  this.firstTabInLine = -1;

  this.documents = [];

  /*
  this.version;
  this.checkLineBreaks;
  this.tagMap;
  this.anchorMap;
  this.tag;
  this.anchor;
  this.kind;
  this.result;*/

}


function generateError(state, message) {
  var mark = {
    name:     state.filename,
    buffer:   state.input.slice(0, -1), // omit trailing \0
    position: state.position,
    line:     state.line,
    column:   state.position - state.lineStart
  };

  mark.snippet = snippet(mark);

  return new exception(message, mark);
}

function throwError(state, message) {
  throw generateError(state, message);
}

function throwWarning(state, message) {
  if (state.onWarning) {
    state.onWarning.call(null, generateError(state, message));
  }
}


var directiveHandlers = {

  YAML: function handleYamlDirective(state, name, args) {

    var match, major, minor;

    if (state.version !== null) {
      throwError(state, 'duplication of %YAML directive');
    }

    if (args.length !== 1) {
      throwError(state, 'YAML directive accepts exactly one argument');
    }

    match = /^([0-9]+)\.([0-9]+)$/.exec(args[0]);

    if (match === null) {
      throwError(state, 'ill-formed argument of the YAML directive');
    }

    major = parseInt(match[1], 10);
    minor = parseInt(match[2], 10);

    if (major !== 1) {
      throwError(state, 'unacceptable YAML version of the document');
    }

    state.version = args[0];
    state.checkLineBreaks = (minor < 2);

    if (minor !== 1 && minor !== 2) {
      throwWarning(state, 'unsupported YAML version of the document');
    }
  },

  TAG: function handleTagDirective(state, name, args) {

    var handle, prefix;

    if (args.length !== 2) {
      throwError(state, 'TAG directive accepts exactly two arguments');
    }

    handle = args[0];
    prefix = args[1];

    if (!PATTERN_TAG_HANDLE.test(handle)) {
      throwError(state, 'ill-formed tag handle (first argument) of the TAG directive');
    }

    if (_hasOwnProperty$1.call(state.tagMap, handle)) {
      throwError(state, 'there is a previously declared suffix for "' + handle + '" tag handle');
    }

    if (!PATTERN_TAG_URI.test(prefix)) {
      throwError(state, 'ill-formed tag prefix (second argument) of the TAG directive');
    }

    try {
      prefix = decodeURIComponent(prefix);
    } catch (err) {
      throwError(state, 'tag prefix is malformed: ' + prefix);
    }

    state.tagMap[handle] = prefix;
  }
};


function captureSegment(state, start, end, checkJson) {
  var _position, _length, _character, _result;

  if (start < end) {
    _result = state.input.slice(start, end);

    if (checkJson) {
      for (_position = 0, _length = _result.length; _position < _length; _position += 1) {
        _character = _result.charCodeAt(_position);
        if (!(_character === 0x09 ||
              (0x20 <= _character && _character <= 0x10FFFF))) {
          throwError(state, 'expected valid JSON character');
        }
      }
    } else if (PATTERN_NON_PRINTABLE.test(_result)) {
      throwError(state, 'the stream contains non-printable characters');
    }

    state.result += _result;
  }
}

function mergeMappings(state, destination, source, overridableKeys) {
  var sourceKeys, key, index, quantity;

  if (!common.isObject(source)) {
    throwError(state, 'cannot merge mappings; the provided source object is unacceptable');
  }

  sourceKeys = Object.keys(source);

  for (index = 0, quantity = sourceKeys.length; index < quantity; index += 1) {
    key = sourceKeys[index];

    if (!_hasOwnProperty$1.call(destination, key)) {
      destination[key] = source[key];
      overridableKeys[key] = true;
    }
  }
}

function storeMappingPair(state, _result, overridableKeys, keyTag, keyNode, valueNode,
  startLine, startLineStart, startPos) {

  var index, quantity;

  // The output is a plain object here, so keys can only be strings.
  // We need to convert keyNode to a string, but doing so can hang the process
  // (deeply nested arrays that explode exponentially using aliases).
  if (Array.isArray(keyNode)) {
    keyNode = Array.prototype.slice.call(keyNode);

    for (index = 0, quantity = keyNode.length; index < quantity; index += 1) {
      if (Array.isArray(keyNode[index])) {
        throwError(state, 'nested arrays are not supported inside keys');
      }

      if (typeof keyNode === 'object' && _class(keyNode[index]) === '[object Object]') {
        keyNode[index] = '[object Object]';
      }
    }
  }

  // Avoid code execution in load() via toString property
  // (still use its own toString for arrays, timestamps,
  // and whatever user schema extensions happen to have @@toStringTag)
  if (typeof keyNode === 'object' && _class(keyNode) === '[object Object]') {
    keyNode = '[object Object]';
  }


  keyNode = String(keyNode);

  if (_result === null) {
    _result = {};
  }

  if (keyTag === 'tag:yaml.org,2002:merge') {
    if (Array.isArray(valueNode)) {
      for (index = 0, quantity = valueNode.length; index < quantity; index += 1) {
        mergeMappings(state, _result, valueNode[index], overridableKeys);
      }
    } else {
      mergeMappings(state, _result, valueNode, overridableKeys);
    }
  } else {
    if (!state.json &&
        !_hasOwnProperty$1.call(overridableKeys, keyNode) &&
        _hasOwnProperty$1.call(_result, keyNode)) {
      state.line = startLine || state.line;
      state.lineStart = startLineStart || state.lineStart;
      state.position = startPos || state.position;
      throwError(state, 'duplicated mapping key');
    }

    // used for this specific key only because Object.defineProperty is slow
    if (keyNode === '__proto__') {
      Object.defineProperty(_result, keyNode, {
        configurable: true,
        enumerable: true,
        writable: true,
        value: valueNode
      });
    } else {
      _result[keyNode] = valueNode;
    }
    delete overridableKeys[keyNode];
  }

  return _result;
}

function readLineBreak(state) {
  var ch;

  ch = state.input.charCodeAt(state.position);

  if (ch === 0x0A/* LF */) {
    state.position++;
  } else if (ch === 0x0D/* CR */) {
    state.position++;
    if (state.input.charCodeAt(state.position) === 0x0A/* LF */) {
      state.position++;
    }
  } else {
    throwError(state, 'a line break is expected');
  }

  state.line += 1;
  state.lineStart = state.position;
  state.firstTabInLine = -1;
}

function skipSeparationSpace(state, allowComments, checkIndent) {
  var lineBreaks = 0,
      ch = state.input.charCodeAt(state.position);

  while (ch !== 0) {
    while (is_WHITE_SPACE(ch)) {
      if (ch === 0x09/* Tab */ && state.firstTabInLine === -1) {
        state.firstTabInLine = state.position;
      }
      ch = state.input.charCodeAt(++state.position);
    }

    if (allowComments && ch === 0x23/* # */) {
      do {
        ch = state.input.charCodeAt(++state.position);
      } while (ch !== 0x0A/* LF */ && ch !== 0x0D/* CR */ && ch !== 0);
    }

    if (is_EOL(ch)) {
      readLineBreak(state);

      ch = state.input.charCodeAt(state.position);
      lineBreaks++;
      state.lineIndent = 0;

      while (ch === 0x20/* Space */) {
        state.lineIndent++;
        ch = state.input.charCodeAt(++state.position);
      }
    } else {
      break;
    }
  }

  if (checkIndent !== -1 && lineBreaks !== 0 && state.lineIndent < checkIndent) {
    throwWarning(state, 'deficient indentation');
  }

  return lineBreaks;
}

function testDocumentSeparator(state) {
  var _position = state.position,
      ch;

  ch = state.input.charCodeAt(_position);

  // Condition state.position === state.lineStart is tested
  // in parent on each call, for efficiency. No needs to test here again.
  if ((ch === 0x2D/* - */ || ch === 0x2E/* . */) &&
      ch === state.input.charCodeAt(_position + 1) &&
      ch === state.input.charCodeAt(_position + 2)) {

    _position += 3;

    ch = state.input.charCodeAt(_position);

    if (ch === 0 || is_WS_OR_EOL(ch)) {
      return true;
    }
  }

  return false;
}

function writeFoldedLines(state, count) {
  if (count === 1) {
    state.result += ' ';
  } else if (count > 1) {
    state.result += common.repeat('\n', count - 1);
  }
}


function readPlainScalar(state, nodeIndent, withinFlowCollection) {
  var preceding,
      following,
      captureStart,
      captureEnd,
      hasPendingContent,
      _line,
      _lineStart,
      _lineIndent,
      _kind = state.kind,
      _result = state.result,
      ch;

  ch = state.input.charCodeAt(state.position);

  if (is_WS_OR_EOL(ch)      ||
      is_FLOW_INDICATOR(ch) ||
      ch === 0x23/* # */    ||
      ch === 0x26/* & */    ||
      ch === 0x2A/* * */    ||
      ch === 0x21/* ! */    ||
      ch === 0x7C/* | */    ||
      ch === 0x3E/* > */    ||
      ch === 0x27/* ' */    ||
      ch === 0x22/* " */    ||
      ch === 0x25/* % */    ||
      ch === 0x40/* @ */    ||
      ch === 0x60/* ` */) {
    return false;
  }

  if (ch === 0x3F/* ? */ || ch === 0x2D/* - */) {
    following = state.input.charCodeAt(state.position + 1);

    if (is_WS_OR_EOL(following) ||
        withinFlowCollection && is_FLOW_INDICATOR(following)) {
      return false;
    }
  }

  state.kind = 'scalar';
  state.result = '';
  captureStart = captureEnd = state.position;
  hasPendingContent = false;

  while (ch !== 0) {
    if (ch === 0x3A/* : */) {
      following = state.input.charCodeAt(state.position + 1);

      if (is_WS_OR_EOL(following) ||
          withinFlowCollection && is_FLOW_INDICATOR(following)) {
        break;
      }

    } else if (ch === 0x23/* # */) {
      preceding = state.input.charCodeAt(state.position - 1);

      if (is_WS_OR_EOL(preceding)) {
        break;
      }

    } else if ((state.position === state.lineStart && testDocumentSeparator(state)) ||
               withinFlowCollection && is_FLOW_INDICATOR(ch)) {
      break;

    } else if (is_EOL(ch)) {
      _line = state.line;
      _lineStart = state.lineStart;
      _lineIndent = state.lineIndent;
      skipSeparationSpace(state, false, -1);

      if (state.lineIndent >= nodeIndent) {
        hasPendingContent = true;
        ch = state.input.charCodeAt(state.position);
        continue;
      } else {
        state.position = captureEnd;
        state.line = _line;
        state.lineStart = _lineStart;
        state.lineIndent = _lineIndent;
        break;
      }
    }

    if (hasPendingContent) {
      captureSegment(state, captureStart, captureEnd, false);
      writeFoldedLines(state, state.line - _line);
      captureStart = captureEnd = state.position;
      hasPendingContent = false;
    }

    if (!is_WHITE_SPACE(ch)) {
      captureEnd = state.position + 1;
    }

    ch = state.input.charCodeAt(++state.position);
  }

  captureSegment(state, captureStart, captureEnd, false);

  if (state.result) {
    return true;
  }

  state.kind = _kind;
  state.result = _result;
  return false;
}

function readSingleQuotedScalar(state, nodeIndent) {
  var ch,
      captureStart, captureEnd;

  ch = state.input.charCodeAt(state.position);

  if (ch !== 0x27/* ' */) {
    return false;
  }

  state.kind = 'scalar';
  state.result = '';
  state.position++;
  captureStart = captureEnd = state.position;

  while ((ch = state.input.charCodeAt(state.position)) !== 0) {
    if (ch === 0x27/* ' */) {
      captureSegment(state, captureStart, state.position, true);
      ch = state.input.charCodeAt(++state.position);

      if (ch === 0x27/* ' */) {
        captureStart = state.position;
        state.position++;
        captureEnd = state.position;
      } else {
        return true;
      }

    } else if (is_EOL(ch)) {
      captureSegment(state, captureStart, captureEnd, true);
      writeFoldedLines(state, skipSeparationSpace(state, false, nodeIndent));
      captureStart = captureEnd = state.position;

    } else if (state.position === state.lineStart && testDocumentSeparator(state)) {
      throwError(state, 'unexpected end of the document within a single quoted scalar');

    } else {
      state.position++;
      captureEnd = state.position;
    }
  }

  throwError(state, 'unexpected end of the stream within a single quoted scalar');
}

function readDoubleQuotedScalar(state, nodeIndent) {
  var captureStart,
      captureEnd,
      hexLength,
      hexResult,
      tmp,
      ch;

  ch = state.input.charCodeAt(state.position);

  if (ch !== 0x22/* " */) {
    return false;
  }

  state.kind = 'scalar';
  state.result = '';
  state.position++;
  captureStart = captureEnd = state.position;

  while ((ch = state.input.charCodeAt(state.position)) !== 0) {
    if (ch === 0x22/* " */) {
      captureSegment(state, captureStart, state.position, true);
      state.position++;
      return true;

    } else if (ch === 0x5C/* \ */) {
      captureSegment(state, captureStart, state.position, true);
      ch = state.input.charCodeAt(++state.position);

      if (is_EOL(ch)) {
        skipSeparationSpace(state, false, nodeIndent);

        // TODO: rework to inline fn with no type cast?
      } else if (ch < 256 && simpleEscapeCheck[ch]) {
        state.result += simpleEscapeMap[ch];
        state.position++;

      } else if ((tmp = escapedHexLen(ch)) > 0) {
        hexLength = tmp;
        hexResult = 0;

        for (; hexLength > 0; hexLength--) {
          ch = state.input.charCodeAt(++state.position);

          if ((tmp = fromHexCode(ch)) >= 0) {
            hexResult = (hexResult << 4) + tmp;

          } else {
            throwError(state, 'expected hexadecimal character');
          }
        }

        state.result += charFromCodepoint(hexResult);

        state.position++;

      } else {
        throwError(state, 'unknown escape sequence');
      }

      captureStart = captureEnd = state.position;

    } else if (is_EOL(ch)) {
      captureSegment(state, captureStart, captureEnd, true);
      writeFoldedLines(state, skipSeparationSpace(state, false, nodeIndent));
      captureStart = captureEnd = state.position;

    } else if (state.position === state.lineStart && testDocumentSeparator(state)) {
      throwError(state, 'unexpected end of the document within a double quoted scalar');

    } else {
      state.position++;
      captureEnd = state.position;
    }
  }

  throwError(state, 'unexpected end of the stream within a double quoted scalar');
}

function readFlowCollection(state, nodeIndent) {
  var readNext = true,
      _line,
      _lineStart,
      _pos,
      _tag     = state.tag,
      _result,
      _anchor  = state.anchor,
      following,
      terminator,
      isPair,
      isExplicitPair,
      isMapping,
      overridableKeys = Object.create(null),
      keyNode,
      keyTag,
      valueNode,
      ch;

  ch = state.input.charCodeAt(state.position);

  if (ch === 0x5B/* [ */) {
    terminator = 0x5D;/* ] */
    isMapping = false;
    _result = [];
  } else if (ch === 0x7B/* { */) {
    terminator = 0x7D;/* } */
    isMapping = true;
    _result = {};
  } else {
    return false;
  }

  if (state.anchor !== null) {
    state.anchorMap[state.anchor] = _result;
  }

  ch = state.input.charCodeAt(++state.position);

  while (ch !== 0) {
    skipSeparationSpace(state, true, nodeIndent);

    ch = state.input.charCodeAt(state.position);

    if (ch === terminator) {
      state.position++;
      state.tag = _tag;
      state.anchor = _anchor;
      state.kind = isMapping ? 'mapping' : 'sequence';
      state.result = _result;
      return true;
    } else if (!readNext) {
      throwError(state, 'missed comma between flow collection entries');
    } else if (ch === 0x2C/* , */) {
      // "flow collection entries can never be completely empty", as per YAML 1.2, section 7.4
      throwError(state, "expected the node content, but found ','");
    }

    keyTag = keyNode = valueNode = null;
    isPair = isExplicitPair = false;

    if (ch === 0x3F/* ? */) {
      following = state.input.charCodeAt(state.position + 1);

      if (is_WS_OR_EOL(following)) {
        isPair = isExplicitPair = true;
        state.position++;
        skipSeparationSpace(state, true, nodeIndent);
      }
    }

    _line = state.line; // Save the current line.
    _lineStart = state.lineStart;
    _pos = state.position;
    composeNode(state, nodeIndent, CONTEXT_FLOW_IN, false, true);
    keyTag = state.tag;
    keyNode = state.result;
    skipSeparationSpace(state, true, nodeIndent);

    ch = state.input.charCodeAt(state.position);

    if ((isExplicitPair || state.line === _line) && ch === 0x3A/* : */) {
      isPair = true;
      ch = state.input.charCodeAt(++state.position);
      skipSeparationSpace(state, true, nodeIndent);
      composeNode(state, nodeIndent, CONTEXT_FLOW_IN, false, true);
      valueNode = state.result;
    }

    if (isMapping) {
      storeMappingPair(state, _result, overridableKeys, keyTag, keyNode, valueNode, _line, _lineStart, _pos);
    } else if (isPair) {
      _result.push(storeMappingPair(state, null, overridableKeys, keyTag, keyNode, valueNode, _line, _lineStart, _pos));
    } else {
      _result.push(keyNode);
    }

    skipSeparationSpace(state, true, nodeIndent);

    ch = state.input.charCodeAt(state.position);

    if (ch === 0x2C/* , */) {
      readNext = true;
      ch = state.input.charCodeAt(++state.position);
    } else {
      readNext = false;
    }
  }

  throwError(state, 'unexpected end of the stream within a flow collection');
}

function readBlockScalar(state, nodeIndent) {
  var captureStart,
      folding,
      chomping       = CHOMPING_CLIP,
      didReadContent = false,
      detectedIndent = false,
      textIndent     = nodeIndent,
      emptyLines     = 0,
      atMoreIndented = false,
      tmp,
      ch;

  ch = state.input.charCodeAt(state.position);

  if (ch === 0x7C/* | */) {
    folding = false;
  } else if (ch === 0x3E/* > */) {
    folding = true;
  } else {
    return false;
  }

  state.kind = 'scalar';
  state.result = '';

  while (ch !== 0) {
    ch = state.input.charCodeAt(++state.position);

    if (ch === 0x2B/* + */ || ch === 0x2D/* - */) {
      if (CHOMPING_CLIP === chomping) {
        chomping = (ch === 0x2B/* + */) ? CHOMPING_KEEP : CHOMPING_STRIP;
      } else {
        throwError(state, 'repeat of a chomping mode identifier');
      }

    } else if ((tmp = fromDecimalCode(ch)) >= 0) {
      if (tmp === 0) {
        throwError(state, 'bad explicit indentation width of a block scalar; it cannot be less than one');
      } else if (!detectedIndent) {
        textIndent = nodeIndent + tmp - 1;
        detectedIndent = true;
      } else {
        throwError(state, 'repeat of an indentation width identifier');
      }

    } else {
      break;
    }
  }

  if (is_WHITE_SPACE(ch)) {
    do { ch = state.input.charCodeAt(++state.position); }
    while (is_WHITE_SPACE(ch));

    if (ch === 0x23/* # */) {
      do { ch = state.input.charCodeAt(++state.position); }
      while (!is_EOL(ch) && (ch !== 0));
    }
  }

  while (ch !== 0) {
    readLineBreak(state);
    state.lineIndent = 0;

    ch = state.input.charCodeAt(state.position);

    while ((!detectedIndent || state.lineIndent < textIndent) &&
           (ch === 0x20/* Space */)) {
      state.lineIndent++;
      ch = state.input.charCodeAt(++state.position);
    }

    if (!detectedIndent && state.lineIndent > textIndent) {
      textIndent = state.lineIndent;
    }

    if (is_EOL(ch)) {
      emptyLines++;
      continue;
    }

    // End of the scalar.
    if (state.lineIndent < textIndent) {

      // Perform the chomping.
      if (chomping === CHOMPING_KEEP) {
        state.result += common.repeat('\n', didReadContent ? 1 + emptyLines : emptyLines);
      } else if (chomping === CHOMPING_CLIP) {
        if (didReadContent) { // i.e. only if the scalar is not empty.
          state.result += '\n';
        }
      }

      // Break this `while` cycle and go to the funciton's epilogue.
      break;
    }

    // Folded style: use fancy rules to handle line breaks.
    if (folding) {

      // Lines starting with white space characters (more-indented lines) are not folded.
      if (is_WHITE_SPACE(ch)) {
        atMoreIndented = true;
        // except for the first content line (cf. Example 8.1)
        state.result += common.repeat('\n', didReadContent ? 1 + emptyLines : emptyLines);

      // End of more-indented block.
      } else if (atMoreIndented) {
        atMoreIndented = false;
        state.result += common.repeat('\n', emptyLines + 1);

      // Just one line break - perceive as the same line.
      } else if (emptyLines === 0) {
        if (didReadContent) { // i.e. only if we have already read some scalar content.
          state.result += ' ';
        }

      // Several line breaks - perceive as different lines.
      } else {
        state.result += common.repeat('\n', emptyLines);
      }

    // Literal style: just add exact number of line breaks between content lines.
    } else {
      // Keep all line breaks except the header line break.
      state.result += common.repeat('\n', didReadContent ? 1 + emptyLines : emptyLines);
    }

    didReadContent = true;
    detectedIndent = true;
    emptyLines = 0;
    captureStart = state.position;

    while (!is_EOL(ch) && (ch !== 0)) {
      ch = state.input.charCodeAt(++state.position);
    }

    captureSegment(state, captureStart, state.position, false);
  }

  return true;
}

function readBlockSequence(state, nodeIndent) {
  var _line,
      _tag      = state.tag,
      _anchor   = state.anchor,
      _result   = [],
      following,
      detected  = false,
      ch;

  // there is a leading tab before this token, so it can't be a block sequence/mapping;
  // it can still be flow sequence/mapping or a scalar
  if (state.firstTabInLine !== -1) return false;

  if (state.anchor !== null) {
    state.anchorMap[state.anchor] = _result;
  }

  ch = state.input.charCodeAt(state.position);

  while (ch !== 0) {
    if (state.firstTabInLine !== -1) {
      state.position = state.firstTabInLine;
      throwError(state, 'tab characters must not be used in indentation');
    }

    if (ch !== 0x2D/* - */) {
      break;
    }

    following = state.input.charCodeAt(state.position + 1);

    if (!is_WS_OR_EOL(following)) {
      break;
    }

    detected = true;
    state.position++;

    if (skipSeparationSpace(state, true, -1)) {
      if (state.lineIndent <= nodeIndent) {
        _result.push(null);
        ch = state.input.charCodeAt(state.position);
        continue;
      }
    }

    _line = state.line;
    composeNode(state, nodeIndent, CONTEXT_BLOCK_IN, false, true);
    _result.push(state.result);
    skipSeparationSpace(state, true, -1);

    ch = state.input.charCodeAt(state.position);

    if ((state.line === _line || state.lineIndent > nodeIndent) && (ch !== 0)) {
      throwError(state, 'bad indentation of a sequence entry');
    } else if (state.lineIndent < nodeIndent) {
      break;
    }
  }

  if (detected) {
    state.tag = _tag;
    state.anchor = _anchor;
    state.kind = 'sequence';
    state.result = _result;
    return true;
  }
  return false;
}

function readBlockMapping(state, nodeIndent, flowIndent) {
  var following,
      allowCompact,
      _line,
      _keyLine,
      _keyLineStart,
      _keyPos,
      _tag          = state.tag,
      _anchor       = state.anchor,
      _result       = {},
      overridableKeys = Object.create(null),
      keyTag        = null,
      keyNode       = null,
      valueNode     = null,
      atExplicitKey = false,
      detected      = false,
      ch;

  // there is a leading tab before this token, so it can't be a block sequence/mapping;
  // it can still be flow sequence/mapping or a scalar
  if (state.firstTabInLine !== -1) return false;

  if (state.anchor !== null) {
    state.anchorMap[state.anchor] = _result;
  }

  ch = state.input.charCodeAt(state.position);

  while (ch !== 0) {
    if (!atExplicitKey && state.firstTabInLine !== -1) {
      state.position = state.firstTabInLine;
      throwError(state, 'tab characters must not be used in indentation');
    }

    following = state.input.charCodeAt(state.position + 1);
    _line = state.line; // Save the current line.

    //
    // Explicit notation case. There are two separate blocks:
    // first for the key (denoted by "?") and second for the value (denoted by ":")
    //
    if ((ch === 0x3F/* ? */ || ch === 0x3A/* : */) && is_WS_OR_EOL(following)) {

      if (ch === 0x3F/* ? */) {
        if (atExplicitKey) {
          storeMappingPair(state, _result, overridableKeys, keyTag, keyNode, null, _keyLine, _keyLineStart, _keyPos);
          keyTag = keyNode = valueNode = null;
        }

        detected = true;
        atExplicitKey = true;
        allowCompact = true;

      } else if (atExplicitKey) {
        // i.e. 0x3A/* : */ === character after the explicit key.
        atExplicitKey = false;
        allowCompact = true;

      } else {
        throwError(state, 'incomplete explicit mapping pair; a key node is missed; or followed by a non-tabulated empty line');
      }

      state.position += 1;
      ch = following;

    //
    // Implicit notation case. Flow-style node as the key first, then ":", and the value.
    //
    } else {
      _keyLine = state.line;
      _keyLineStart = state.lineStart;
      _keyPos = state.position;

      if (!composeNode(state, flowIndent, CONTEXT_FLOW_OUT, false, true)) {
        // Neither implicit nor explicit notation.
        // Reading is done. Go to the epilogue.
        break;
      }

      if (state.line === _line) {
        ch = state.input.charCodeAt(state.position);

        while (is_WHITE_SPACE(ch)) {
          ch = state.input.charCodeAt(++state.position);
        }

        if (ch === 0x3A/* : */) {
          ch = state.input.charCodeAt(++state.position);

          if (!is_WS_OR_EOL(ch)) {
            throwError(state, 'a whitespace character is expected after the key-value separator within a block mapping');
          }

          if (atExplicitKey) {
            storeMappingPair(state, _result, overridableKeys, keyTag, keyNode, null, _keyLine, _keyLineStart, _keyPos);
            keyTag = keyNode = valueNode = null;
          }

          detected = true;
          atExplicitKey = false;
          allowCompact = false;
          keyTag = state.tag;
          keyNode = state.result;

        } else if (detected) {
          throwError(state, 'can not read an implicit mapping pair; a colon is missed');

        } else {
          state.tag = _tag;
          state.anchor = _anchor;
          return true; // Keep the result of `composeNode`.
        }

      } else if (detected) {
        throwError(state, 'can not read a block mapping entry; a multiline key may not be an implicit key');

      } else {
        state.tag = _tag;
        state.anchor = _anchor;
        return true; // Keep the result of `composeNode`.
      }
    }

    //
    // Common reading code for both explicit and implicit notations.
    //
    if (state.line === _line || state.lineIndent > nodeIndent) {
      if (atExplicitKey) {
        _keyLine = state.line;
        _keyLineStart = state.lineStart;
        _keyPos = state.position;
      }

      if (composeNode(state, nodeIndent, CONTEXT_BLOCK_OUT, true, allowCompact)) {
        if (atExplicitKey) {
          keyNode = state.result;
        } else {
          valueNode = state.result;
        }
      }

      if (!atExplicitKey) {
        storeMappingPair(state, _result, overridableKeys, keyTag, keyNode, valueNode, _keyLine, _keyLineStart, _keyPos);
        keyTag = keyNode = valueNode = null;
      }

      skipSeparationSpace(state, true, -1);
      ch = state.input.charCodeAt(state.position);
    }

    if ((state.line === _line || state.lineIndent > nodeIndent) && (ch !== 0)) {
      throwError(state, 'bad indentation of a mapping entry');
    } else if (state.lineIndent < nodeIndent) {
      break;
    }
  }

  //
  // Epilogue.
  //

  // Special case: last mapping's node contains only the key in explicit notation.
  if (atExplicitKey) {
    storeMappingPair(state, _result, overridableKeys, keyTag, keyNode, null, _keyLine, _keyLineStart, _keyPos);
  }

  // Expose the resulting mapping.
  if (detected) {
    state.tag = _tag;
    state.anchor = _anchor;
    state.kind = 'mapping';
    state.result = _result;
  }

  return detected;
}

function readTagProperty(state) {
  var _position,
      isVerbatim = false,
      isNamed    = false,
      tagHandle,
      tagName,
      ch;

  ch = state.input.charCodeAt(state.position);

  if (ch !== 0x21/* ! */) return false;

  if (state.tag !== null) {
    throwError(state, 'duplication of a tag property');
  }

  ch = state.input.charCodeAt(++state.position);

  if (ch === 0x3C/* < */) {
    isVerbatim = true;
    ch = state.input.charCodeAt(++state.position);

  } else if (ch === 0x21/* ! */) {
    isNamed = true;
    tagHandle = '!!';
    ch = state.input.charCodeAt(++state.position);

  } else {
    tagHandle = '!';
  }

  _position = state.position;

  if (isVerbatim) {
    do { ch = state.input.charCodeAt(++state.position); }
    while (ch !== 0 && ch !== 0x3E/* > */);

    if (state.position < state.length) {
      tagName = state.input.slice(_position, state.position);
      ch = state.input.charCodeAt(++state.position);
    } else {
      throwError(state, 'unexpected end of the stream within a verbatim tag');
    }
  } else {
    while (ch !== 0 && !is_WS_OR_EOL(ch)) {

      if (ch === 0x21/* ! */) {
        if (!isNamed) {
          tagHandle = state.input.slice(_position - 1, state.position + 1);

          if (!PATTERN_TAG_HANDLE.test(tagHandle)) {
            throwError(state, 'named tag handle cannot contain such characters');
          }

          isNamed = true;
          _position = state.position + 1;
        } else {
          throwError(state, 'tag suffix cannot contain exclamation marks');
        }
      }

      ch = state.input.charCodeAt(++state.position);
    }

    tagName = state.input.slice(_position, state.position);

    if (PATTERN_FLOW_INDICATORS.test(tagName)) {
      throwError(state, 'tag suffix cannot contain flow indicator characters');
    }
  }

  if (tagName && !PATTERN_TAG_URI.test(tagName)) {
    throwError(state, 'tag name cannot contain such characters: ' + tagName);
  }

  try {
    tagName = decodeURIComponent(tagName);
  } catch (err) {
    throwError(state, 'tag name is malformed: ' + tagName);
  }

  if (isVerbatim) {
    state.tag = tagName;

  } else if (_hasOwnProperty$1.call(state.tagMap, tagHandle)) {
    state.tag = state.tagMap[tagHandle] + tagName;

  } else if (tagHandle === '!') {
    state.tag = '!' + tagName;

  } else if (tagHandle === '!!') {
    state.tag = 'tag:yaml.org,2002:' + tagName;

  } else {
    throwError(state, 'undeclared tag handle "' + tagHandle + '"');
  }

  return true;
}

function readAnchorProperty(state) {
  var _position,
      ch;

  ch = state.input.charCodeAt(state.position);

  if (ch !== 0x26/* & */) return false;

  if (state.anchor !== null) {
    throwError(state, 'duplication of an anchor property');
  }

  ch = state.input.charCodeAt(++state.position);
  _position = state.position;

  while (ch !== 0 && !is_WS_OR_EOL(ch) && !is_FLOW_INDICATOR(ch)) {
    ch = state.input.charCodeAt(++state.position);
  }

  if (state.position === _position) {
    throwError(state, 'name of an anchor node must contain at least one character');
  }

  state.anchor = state.input.slice(_position, state.position);
  return true;
}

function readAlias(state) {
  var _position, alias,
      ch;

  ch = state.input.charCodeAt(state.position);

  if (ch !== 0x2A/* * */) return false;

  ch = state.input.charCodeAt(++state.position);
  _position = state.position;

  while (ch !== 0 && !is_WS_OR_EOL(ch) && !is_FLOW_INDICATOR(ch)) {
    ch = state.input.charCodeAt(++state.position);
  }

  if (state.position === _position) {
    throwError(state, 'name of an alias node must contain at least one character');
  }

  alias = state.input.slice(_position, state.position);

  if (!_hasOwnProperty$1.call(state.anchorMap, alias)) {
    throwError(state, 'unidentified alias "' + alias + '"');
  }

  state.result = state.anchorMap[alias];
  skipSeparationSpace(state, true, -1);
  return true;
}

function composeNode(state, parentIndent, nodeContext, allowToSeek, allowCompact) {
  var allowBlockStyles,
      allowBlockScalars,
      allowBlockCollections,
      indentStatus = 1, // 1: this>parent, 0: this=parent, -1: this<parent
      atNewLine  = false,
      hasContent = false,
      typeIndex,
      typeQuantity,
      typeList,
      type,
      flowIndent,
      blockIndent;

  if (state.listener !== null) {
    state.listener('open', state);
  }

  state.tag    = null;
  state.anchor = null;
  state.kind   = null;
  state.result = null;

  allowBlockStyles = allowBlockScalars = allowBlockCollections =
    CONTEXT_BLOCK_OUT === nodeContext ||
    CONTEXT_BLOCK_IN  === nodeContext;

  if (allowToSeek) {
    if (skipSeparationSpace(state, true, -1)) {
      atNewLine = true;

      if (state.lineIndent > parentIndent) {
        indentStatus = 1;
      } else if (state.lineIndent === parentIndent) {
        indentStatus = 0;
      } else if (state.lineIndent < parentIndent) {
        indentStatus = -1;
      }
    }
  }

  if (indentStatus === 1) {
    while (readTagProperty(state) || readAnchorProperty(state)) {
      if (skipSeparationSpace(state, true, -1)) {
        atNewLine = true;
        allowBlockCollections = allowBlockStyles;

        if (state.lineIndent > parentIndent) {
          indentStatus = 1;
        } else if (state.lineIndent === parentIndent) {
          indentStatus = 0;
        } else if (state.lineIndent < parentIndent) {
          indentStatus = -1;
        }
      } else {
        allowBlockCollections = false;
      }
    }
  }

  if (allowBlockCollections) {
    allowBlockCollections = atNewLine || allowCompact;
  }

  if (indentStatus === 1 || CONTEXT_BLOCK_OUT === nodeContext) {
    if (CONTEXT_FLOW_IN === nodeContext || CONTEXT_FLOW_OUT === nodeContext) {
      flowIndent = parentIndent;
    } else {
      flowIndent = parentIndent + 1;
    }

    blockIndent = state.position - state.lineStart;

    if (indentStatus === 1) {
      if (allowBlockCollections &&
          (readBlockSequence(state, blockIndent) ||
           readBlockMapping(state, blockIndent, flowIndent)) ||
          readFlowCollection(state, flowIndent)) {
        hasContent = true;
      } else {
        if ((allowBlockScalars && readBlockScalar(state, flowIndent)) ||
            readSingleQuotedScalar(state, flowIndent) ||
            readDoubleQuotedScalar(state, flowIndent)) {
          hasContent = true;

        } else if (readAlias(state)) {
          hasContent = true;

          if (state.tag !== null || state.anchor !== null) {
            throwError(state, 'alias node should not have any properties');
          }

        } else if (readPlainScalar(state, flowIndent, CONTEXT_FLOW_IN === nodeContext)) {
          hasContent = true;

          if (state.tag === null) {
            state.tag = '?';
          }
        }

        if (state.anchor !== null) {
          state.anchorMap[state.anchor] = state.result;
        }
      }
    } else if (indentStatus === 0) {
      // Special case: block sequences are allowed to have same indentation level as the parent.
      // http://www.yaml.org/spec/1.2/spec.html#id2799784
      hasContent = allowBlockCollections && readBlockSequence(state, blockIndent);
    }
  }

  if (state.tag === null) {
    if (state.anchor !== null) {
      state.anchorMap[state.anchor] = state.result;
    }

  } else if (state.tag === '?') {
    // Implicit resolving is not allowed for non-scalar types, and '?'
    // non-specific tag is only automatically assigned to plain scalars.
    //
    // We only need to check kind conformity in case user explicitly assigns '?'
    // tag, for example like this: "!<?> [0]"
    //
    if (state.result !== null && state.kind !== 'scalar') {
      throwError(state, 'unacceptable node kind for !<?> tag; it should be "scalar", not "' + state.kind + '"');
    }

    for (typeIndex = 0, typeQuantity = state.implicitTypes.length; typeIndex < typeQuantity; typeIndex += 1) {
      type = state.implicitTypes[typeIndex];

      if (type.resolve(state.result)) { // `state.result` updated in resolver if matched
        state.result = type.construct(state.result);
        state.tag = type.tag;
        if (state.anchor !== null) {
          state.anchorMap[state.anchor] = state.result;
        }
        break;
      }
    }
  } else if (state.tag !== '!') {
    if (_hasOwnProperty$1.call(state.typeMap[state.kind || 'fallback'], state.tag)) {
      type = state.typeMap[state.kind || 'fallback'][state.tag];
    } else {
      // looking for multi type
      type = null;
      typeList = state.typeMap.multi[state.kind || 'fallback'];

      for (typeIndex = 0, typeQuantity = typeList.length; typeIndex < typeQuantity; typeIndex += 1) {
        if (state.tag.slice(0, typeList[typeIndex].tag.length) === typeList[typeIndex].tag) {
          type = typeList[typeIndex];
          break;
        }
      }
    }

    if (!type) {
      throwError(state, 'unknown tag !<' + state.tag + '>');
    }

    if (state.result !== null && type.kind !== state.kind) {
      throwError(state, 'unacceptable node kind for !<' + state.tag + '> tag; it should be "' + type.kind + '", not "' + state.kind + '"');
    }

    if (!type.resolve(state.result, state.tag)) { // `state.result` updated in resolver if matched
      throwError(state, 'cannot resolve a node with !<' + state.tag + '> explicit tag');
    } else {
      state.result = type.construct(state.result, state.tag);
      if (state.anchor !== null) {
        state.anchorMap[state.anchor] = state.result;
      }
    }
  }

  if (state.listener !== null) {
    state.listener('close', state);
  }
  return state.tag !== null ||  state.anchor !== null || hasContent;
}

function readDocument(state) {
  var documentStart = state.position,
      _position,
      directiveName,
      directiveArgs,
      hasDirectives = false,
      ch;

  state.version = null;
  state.checkLineBreaks = state.legacy;
  state.tagMap = Object.create(null);
  state.anchorMap = Object.create(null);

  while ((ch = state.input.charCodeAt(state.position)) !== 0) {
    skipSeparationSpace(state, true, -1);

    ch = state.input.charCodeAt(state.position);

    if (state.lineIndent > 0 || ch !== 0x25/* % */) {
      break;
    }

    hasDirectives = true;
    ch = state.input.charCodeAt(++state.position);
    _position = state.position;

    while (ch !== 0 && !is_WS_OR_EOL(ch)) {
      ch = state.input.charCodeAt(++state.position);
    }

    directiveName = state.input.slice(_position, state.position);
    directiveArgs = [];

    if (directiveName.length < 1) {
      throwError(state, 'directive name must not be less than one character in length');
    }

    while (ch !== 0) {
      while (is_WHITE_SPACE(ch)) {
        ch = state.input.charCodeAt(++state.position);
      }

      if (ch === 0x23/* # */) {
        do { ch = state.input.charCodeAt(++state.position); }
        while (ch !== 0 && !is_EOL(ch));
        break;
      }

      if (is_EOL(ch)) break;

      _position = state.position;

      while (ch !== 0 && !is_WS_OR_EOL(ch)) {
        ch = state.input.charCodeAt(++state.position);
      }

      directiveArgs.push(state.input.slice(_position, state.position));
    }

    if (ch !== 0) readLineBreak(state);

    if (_hasOwnProperty$1.call(directiveHandlers, directiveName)) {
      directiveHandlers[directiveName](state, directiveName, directiveArgs);
    } else {
      throwWarning(state, 'unknown document directive "' + directiveName + '"');
    }
  }

  skipSeparationSpace(state, true, -1);

  if (state.lineIndent === 0 &&
      state.input.charCodeAt(state.position)     === 0x2D/* - */ &&
      state.input.charCodeAt(state.position + 1) === 0x2D/* - */ &&
      state.input.charCodeAt(state.position + 2) === 0x2D/* - */) {
    state.position += 3;
    skipSeparationSpace(state, true, -1);

  } else if (hasDirectives) {
    throwError(state, 'directives end mark is expected');
  }

  composeNode(state, state.lineIndent - 1, CONTEXT_BLOCK_OUT, false, true);
  skipSeparationSpace(state, true, -1);

  if (state.checkLineBreaks &&
      PATTERN_NON_ASCII_LINE_BREAKS.test(state.input.slice(documentStart, state.position))) {
    throwWarning(state, 'non-ASCII line breaks are interpreted as content');
  }

  state.documents.push(state.result);

  if (state.position === state.lineStart && testDocumentSeparator(state)) {

    if (state.input.charCodeAt(state.position) === 0x2E/* . */) {
      state.position += 3;
      skipSeparationSpace(state, true, -1);
    }
    return;
  }

  if (state.position < (state.length - 1)) {
    throwError(state, 'end of the stream or a document separator is expected');
  } else {
    return;
  }
}


function loadDocuments(input, options) {
  input = String(input);
  options = options || {};

  if (input.length !== 0) {

    // Add tailing `\n` if not exists
    if (input.charCodeAt(input.length - 1) !== 0x0A/* LF */ &&
        input.charCodeAt(input.length - 1) !== 0x0D/* CR */) {
      input += '\n';
    }

    // Strip BOM
    if (input.charCodeAt(0) === 0xFEFF) {
      input = input.slice(1);
    }
  }

  var state = new State$1(input, options);

  var nullpos = input.indexOf('\0');

  if (nullpos !== -1) {
    state.position = nullpos;
    throwError(state, 'null byte is not allowed in input');
  }

  // Use 0 as string terminator. That significantly simplifies bounds check.
  state.input += '\0';

  while (state.input.charCodeAt(state.position) === 0x20/* Space */) {
    state.lineIndent += 1;
    state.position += 1;
  }

  while (state.position < (state.length - 1)) {
    readDocument(state);
  }

  return state.documents;
}


function loadAll$1(input, iterator, options) {
  if (iterator !== null && typeof iterator === 'object' && typeof options === 'undefined') {
    options = iterator;
    iterator = null;
  }

  var documents = loadDocuments(input, options);

  if (typeof iterator !== 'function') {
    return documents;
  }

  for (var index = 0, length = documents.length; index < length; index += 1) {
    iterator(documents[index]);
  }
}


function load$1(input, options) {
  var documents = loadDocuments(input, options);

  if (documents.length === 0) {
    /*eslint-disable no-undefined*/
    return undefined;
  } else if (documents.length === 1) {
    return documents[0];
  }
  throw new exception('expected a single document in the stream, but found more');
}


var loadAll_1 = loadAll$1;
var load_1    = load$1;

var loader = {
	loadAll: loadAll_1,
	load: load_1
};

/*eslint-disable no-use-before-define*/





var _toString       = Object.prototype.toString;
var _hasOwnProperty = Object.prototype.hasOwnProperty;

var CHAR_BOM                  = 0xFEFF;
var CHAR_TAB                  = 0x09; /* Tab */
var CHAR_LINE_FEED            = 0x0A; /* LF */
var CHAR_CARRIAGE_RETURN      = 0x0D; /* CR */
var CHAR_SPACE                = 0x20; /* Space */
var CHAR_EXCLAMATION          = 0x21; /* ! */
var CHAR_DOUBLE_QUOTE         = 0x22; /* " */
var CHAR_SHARP                = 0x23; /* # */
var CHAR_PERCENT              = 0x25; /* % */
var CHAR_AMPERSAND            = 0x26; /* & */
var CHAR_SINGLE_QUOTE         = 0x27; /* ' */
var CHAR_ASTERISK             = 0x2A; /* * */
var CHAR_COMMA                = 0x2C; /* , */
var CHAR_MINUS                = 0x2D; /* - */
var CHAR_COLON                = 0x3A; /* : */
var CHAR_EQUALS               = 0x3D; /* = */
var CHAR_GREATER_THAN         = 0x3E; /* > */
var CHAR_QUESTION             = 0x3F; /* ? */
var CHAR_COMMERCIAL_AT        = 0x40; /* @ */
var CHAR_LEFT_SQUARE_BRACKET  = 0x5B; /* [ */
var CHAR_RIGHT_SQUARE_BRACKET = 0x5D; /* ] */
var CHAR_GRAVE_ACCENT         = 0x60; /* ` */
var CHAR_LEFT_CURLY_BRACKET   = 0x7B; /* { */
var CHAR_VERTICAL_LINE        = 0x7C; /* | */
var CHAR_RIGHT_CURLY_BRACKET  = 0x7D; /* } */

var ESCAPE_SEQUENCES = {};

ESCAPE_SEQUENCES[0x00]   = '\\0';
ESCAPE_SEQUENCES[0x07]   = '\\a';
ESCAPE_SEQUENCES[0x08]   = '\\b';
ESCAPE_SEQUENCES[0x09]   = '\\t';
ESCAPE_SEQUENCES[0x0A]   = '\\n';
ESCAPE_SEQUENCES[0x0B]   = '\\v';
ESCAPE_SEQUENCES[0x0C]   = '\\f';
ESCAPE_SEQUENCES[0x0D]   = '\\r';
ESCAPE_SEQUENCES[0x1B]   = '\\e';
ESCAPE_SEQUENCES[0x22]   = '\\"';
ESCAPE_SEQUENCES[0x5C]   = '\\\\';
ESCAPE_SEQUENCES[0x85]   = '\\N';
ESCAPE_SEQUENCES[0xA0]   = '\\_';
ESCAPE_SEQUENCES[0x2028] = '\\L';
ESCAPE_SEQUENCES[0x2029] = '\\P';

var DEPRECATED_BOOLEANS_SYNTAX = [
  'y', 'Y', 'yes', 'Yes', 'YES', 'on', 'On', 'ON',
  'n', 'N', 'no', 'No', 'NO', 'off', 'Off', 'OFF'
];

var DEPRECATED_BASE60_SYNTAX = /^[-+]?[0-9_]+(?::[0-9_]+)+(?:\.[0-9_]*)?$/;

function compileStyleMap(schema, map) {
  var result, keys, index, length, tag, style, type;

  if (map === null) return {};

  result = {};
  keys = Object.keys(map);

  for (index = 0, length = keys.length; index < length; index += 1) {
    tag = keys[index];
    style = String(map[tag]);

    if (tag.slice(0, 2) === '!!') {
      tag = 'tag:yaml.org,2002:' + tag.slice(2);
    }
    type = schema.compiledTypeMap['fallback'][tag];

    if (type && _hasOwnProperty.call(type.styleAliases, style)) {
      style = type.styleAliases[style];
    }

    result[tag] = style;
  }

  return result;
}

function encodeHex(character) {
  var string, handle, length;

  string = character.toString(16).toUpperCase();

  if (character <= 0xFF) {
    handle = 'x';
    length = 2;
  } else if (character <= 0xFFFF) {
    handle = 'u';
    length = 4;
  } else if (character <= 0xFFFFFFFF) {
    handle = 'U';
    length = 8;
  } else {
    throw new exception('code point within a string may not be greater than 0xFFFFFFFF');
  }

  return '\\' + handle + common.repeat('0', length - string.length) + string;
}


var QUOTING_TYPE_SINGLE = 1,
    QUOTING_TYPE_DOUBLE = 2;

function State(options) {
  this.schema        = options['schema'] || _default;
  this.indent        = Math.max(1, (options['indent'] || 2));
  this.noArrayIndent = options['noArrayIndent'] || false;
  this.skipInvalid   = options['skipInvalid'] || false;
  this.flowLevel     = (common.isNothing(options['flowLevel']) ? -1 : options['flowLevel']);
  this.styleMap      = compileStyleMap(this.schema, options['styles'] || null);
  this.sortKeys      = options['sortKeys'] || false;
  this.lineWidth     = options['lineWidth'] || 80;
  this.noRefs        = options['noRefs'] || false;
  this.noCompatMode  = options['noCompatMode'] || false;
  this.condenseFlow  = options['condenseFlow'] || false;
  this.quotingType   = options['quotingType'] === '"' ? QUOTING_TYPE_DOUBLE : QUOTING_TYPE_SINGLE;
  this.forceQuotes   = options['forceQuotes'] || false;
  this.replacer      = typeof options['replacer'] === 'function' ? options['replacer'] : null;

  this.implicitTypes = this.schema.compiledImplicit;
  this.explicitTypes = this.schema.compiledExplicit;

  this.tag = null;
  this.result = '';

  this.duplicates = [];
  this.usedDuplicates = null;
}

// Indents every line in a string. Empty lines (\n only) are not indented.
function indentString(string, spaces) {
  var ind = common.repeat(' ', spaces),
      position = 0,
      next = -1,
      result = '',
      line,
      length = string.length;

  while (position < length) {
    next = string.indexOf('\n', position);
    if (next === -1) {
      line = string.slice(position);
      position = length;
    } else {
      line = string.slice(position, next + 1);
      position = next + 1;
    }

    if (line.length && line !== '\n') result += ind;

    result += line;
  }

  return result;
}

function generateNextLine(state, level) {
  return '\n' + common.repeat(' ', state.indent * level);
}

function testImplicitResolving(state, str) {
  var index, length, type;

  for (index = 0, length = state.implicitTypes.length; index < length; index += 1) {
    type = state.implicitTypes[index];

    if (type.resolve(str)) {
      return true;
    }
  }

  return false;
}

// [33] s-white ::= s-space | s-tab
function isWhitespace(c) {
  return c === CHAR_SPACE || c === CHAR_TAB;
}

// Returns true if the character can be printed without escaping.
// From YAML 1.2: "any allowed characters known to be non-printable
// should also be escaped. [However,] This isn’t mandatory"
// Derived from nb-char - \t - #x85 - #xA0 - #x2028 - #x2029.
function isPrintable(c) {
  return  (0x00020 <= c && c <= 0x00007E)
      || ((0x000A1 <= c && c <= 0x00D7FF) && c !== 0x2028 && c !== 0x2029)
      || ((0x0E000 <= c && c <= 0x00FFFD) && c !== CHAR_BOM)
      ||  (0x10000 <= c && c <= 0x10FFFF);
}

// [34] ns-char ::= nb-char - s-white
// [27] nb-char ::= c-printable - b-char - c-byte-order-mark
// [26] b-char  ::= b-line-feed | b-carriage-return
// Including s-white (for some reason, examples doesn't match specs in this aspect)
// ns-char ::= c-printable - b-line-feed - b-carriage-return - c-byte-order-mark
function isNsCharOrWhitespace(c) {
  return isPrintable(c)
    && c !== CHAR_BOM
    // - b-char
    && c !== CHAR_CARRIAGE_RETURN
    && c !== CHAR_LINE_FEED;
}

// [127]  ns-plain-safe(c) ::= c = flow-out  ⇒ ns-plain-safe-out
//                             c = flow-in   ⇒ ns-plain-safe-in
//                             c = block-key ⇒ ns-plain-safe-out
//                             c = flow-key  ⇒ ns-plain-safe-in
// [128] ns-plain-safe-out ::= ns-char
// [129]  ns-plain-safe-in ::= ns-char - c-flow-indicator
// [130]  ns-plain-char(c) ::=  ( ns-plain-safe(c) - “:” - “#” )
//                            | ( /* An ns-char preceding */ “#” )
//                            | ( “:” /* Followed by an ns-plain-safe(c) */ )
function isPlainSafe(c, prev, inblock) {
  var cIsNsCharOrWhitespace = isNsCharOrWhitespace(c);
  var cIsNsChar = cIsNsCharOrWhitespace && !isWhitespace(c);
  return (
    // ns-plain-safe
    inblock ? // c = flow-in
      cIsNsCharOrWhitespace
      : cIsNsCharOrWhitespace
        // - c-flow-indicator
        && c !== CHAR_COMMA
        && c !== CHAR_LEFT_SQUARE_BRACKET
        && c !== CHAR_RIGHT_SQUARE_BRACKET
        && c !== CHAR_LEFT_CURLY_BRACKET
        && c !== CHAR_RIGHT_CURLY_BRACKET
  )
    // ns-plain-char
    && c !== CHAR_SHARP // false on '#'
    && !(prev === CHAR_COLON && !cIsNsChar) // false on ': '
    || (isNsCharOrWhitespace(prev) && !isWhitespace(prev) && c === CHAR_SHARP) // change to true on '[^ ]#'
    || (prev === CHAR_COLON && cIsNsChar); // change to true on ':[^ ]'
}

// Simplified test for values allowed as the first character in plain style.
function isPlainSafeFirst(c) {
  // Uses a subset of ns-char - c-indicator
  // where ns-char = nb-char - s-white.
  // No support of ( ( “?” | “:” | “-” ) /* Followed by an ns-plain-safe(c)) */ ) part
  return isPrintable(c) && c !== CHAR_BOM
    && !isWhitespace(c) // - s-white
    // - (c-indicator ::=
    // “-” | “?” | “:” | “,” | “[” | “]” | “{” | “}”
    && c !== CHAR_MINUS
    && c !== CHAR_QUESTION
    && c !== CHAR_COLON
    && c !== CHAR_COMMA
    && c !== CHAR_LEFT_SQUARE_BRACKET
    && c !== CHAR_RIGHT_SQUARE_BRACKET
    && c !== CHAR_LEFT_CURLY_BRACKET
    && c !== CHAR_RIGHT_CURLY_BRACKET
    // | “#” | “&” | “*” | “!” | “|” | “=” | “>” | “'” | “"”
    && c !== CHAR_SHARP
    && c !== CHAR_AMPERSAND
    && c !== CHAR_ASTERISK
    && c !== CHAR_EXCLAMATION
    && c !== CHAR_VERTICAL_LINE
    && c !== CHAR_EQUALS
    && c !== CHAR_GREATER_THAN
    && c !== CHAR_SINGLE_QUOTE
    && c !== CHAR_DOUBLE_QUOTE
    // | “%” | “@” | “`”)
    && c !== CHAR_PERCENT
    && c !== CHAR_COMMERCIAL_AT
    && c !== CHAR_GRAVE_ACCENT;
}

// Simplified test for values allowed as the last character in plain style.
function isPlainSafeLast(c) {
  // just not whitespace or colon, it will be checked to be plain character later
  return !isWhitespace(c) && c !== CHAR_COLON;
}

// Same as 'string'.codePointAt(pos), but works in older browsers.
function codePointAt(string, pos) {
  var first = string.charCodeAt(pos), second;
  if (first >= 0xD800 && first <= 0xDBFF && pos + 1 < string.length) {
    second = string.charCodeAt(pos + 1);
    if (second >= 0xDC00 && second <= 0xDFFF) {
      // https://mathiasbynens.be/notes/javascript-encoding#surrogate-formulae
      return (first - 0xD800) * 0x400 + second - 0xDC00 + 0x10000;
    }
  }
  return first;
}

// Determines whether block indentation indicator is required.
function needIndentIndicator(string) {
  var leadingSpaceRe = /^\n* /;
  return leadingSpaceRe.test(string);
}

var STYLE_PLAIN   = 1,
    STYLE_SINGLE  = 2,
    STYLE_LITERAL = 3,
    STYLE_FOLDED  = 4,
    STYLE_DOUBLE  = 5;

// Determines which scalar styles are possible and returns the preferred style.
// lineWidth = -1 => no limit.
// Pre-conditions: str.length > 0.
// Post-conditions:
//    STYLE_PLAIN or STYLE_SINGLE => no \n are in the string.
//    STYLE_LITERAL => no lines are suitable for folding (or lineWidth is -1).
//    STYLE_FOLDED => a line > lineWidth and can be folded (and lineWidth != -1).
function chooseScalarStyle(string, singleLineOnly, indentPerLevel, lineWidth,
  testAmbiguousType, quotingType, forceQuotes, inblock) {

  var i;
  var char = 0;
  var prevChar = null;
  var hasLineBreak = false;
  var hasFoldableLine = false; // only checked if shouldTrackWidth
  var shouldTrackWidth = lineWidth !== -1;
  var previousLineBreak = -1; // count the first line correctly
  var plain = isPlainSafeFirst(codePointAt(string, 0))
          && isPlainSafeLast(codePointAt(string, string.length - 1));

  if (singleLineOnly || forceQuotes) {
    // Case: no block styles.
    // Check for disallowed characters to rule out plain and single.
    for (i = 0; i < string.length; char >= 0x10000 ? i += 2 : i++) {
      char = codePointAt(string, i);
      if (!isPrintable(char)) {
        return STYLE_DOUBLE;
      }
      plain = plain && isPlainSafe(char, prevChar, inblock);
      prevChar = char;
    }
  } else {
    // Case: block styles permitted.
    for (i = 0; i < string.length; char >= 0x10000 ? i += 2 : i++) {
      char = codePointAt(string, i);
      if (char === CHAR_LINE_FEED) {
        hasLineBreak = true;
        // Check if any line can be folded.
        if (shouldTrackWidth) {
          hasFoldableLine = hasFoldableLine ||
            // Foldable line = too long, and not more-indented.
            (i - previousLineBreak - 1 > lineWidth &&
             string[previousLineBreak + 1] !== ' ');
          previousLineBreak = i;
        }
      } else if (!isPrintable(char)) {
        return STYLE_DOUBLE;
      }
      plain = plain && isPlainSafe(char, prevChar, inblock);
      prevChar = char;
    }
    // in case the end is missing a \n
    hasFoldableLine = hasFoldableLine || (shouldTrackWidth &&
      (i - previousLineBreak - 1 > lineWidth &&
       string[previousLineBreak + 1] !== ' '));
  }
  // Although every style can represent \n without escaping, prefer block styles
  // for multiline, since they're more readable and they don't add empty lines.
  // Also prefer folding a super-long line.
  if (!hasLineBreak && !hasFoldableLine) {
    // Strings interpretable as another type have to be quoted;
    // e.g. the string 'true' vs. the boolean true.
    if (plain && !forceQuotes && !testAmbiguousType(string)) {
      return STYLE_PLAIN;
    }
    return quotingType === QUOTING_TYPE_DOUBLE ? STYLE_DOUBLE : STYLE_SINGLE;
  }
  // Edge case: block indentation indicator can only have one digit.
  if (indentPerLevel > 9 && needIndentIndicator(string)) {
    return STYLE_DOUBLE;
  }
  // At this point we know block styles are valid.
  // Prefer literal style unless we want to fold.
  if (!forceQuotes) {
    return hasFoldableLine ? STYLE_FOLDED : STYLE_LITERAL;
  }
  return quotingType === QUOTING_TYPE_DOUBLE ? STYLE_DOUBLE : STYLE_SINGLE;
}

// Note: line breaking/folding is implemented for only the folded style.
// NB. We drop the last trailing newline (if any) of a returned block scalar
//  since the dumper adds its own newline. This always works:
//    • No ending newline => unaffected; already using strip "-" chomping.
//    • Ending newline    => removed then restored.
//  Importantly, this keeps the "+" chomp indicator from gaining an extra line.
function writeScalar(state, string, level, iskey, inblock) {
  state.dump = (function () {
    if (string.length === 0) {
      return state.quotingType === QUOTING_TYPE_DOUBLE ? '""' : "''";
    }
    if (!state.noCompatMode) {
      if (DEPRECATED_BOOLEANS_SYNTAX.indexOf(string) !== -1 || DEPRECATED_BASE60_SYNTAX.test(string)) {
        return state.quotingType === QUOTING_TYPE_DOUBLE ? ('"' + string + '"') : ("'" + string + "'");
      }
    }

    var indent = state.indent * Math.max(1, level); // no 0-indent scalars
    // As indentation gets deeper, let the width decrease monotonically
    // to the lower bound min(state.lineWidth, 40).
    // Note that this implies
    //  state.lineWidth ≤ 40 + state.indent: width is fixed at the lower bound.
    //  state.lineWidth > 40 + state.indent: width decreases until the lower bound.
    // This behaves better than a constant minimum width which disallows narrower options,
    // or an indent threshold which causes the width to suddenly increase.
    var lineWidth = state.lineWidth === -1
      ? -1 : Math.max(Math.min(state.lineWidth, 40), state.lineWidth - indent);

    // Without knowing if keys are implicit/explicit, assume implicit for safety.
    var singleLineOnly = iskey
      // No block styles in flow mode.
      || (state.flowLevel > -1 && level >= state.flowLevel);
    function testAmbiguity(string) {
      return testImplicitResolving(state, string);
    }

    switch (chooseScalarStyle(string, singleLineOnly, state.indent, lineWidth,
      testAmbiguity, state.quotingType, state.forceQuotes && !iskey, inblock)) {

      case STYLE_PLAIN:
        return string;
      case STYLE_SINGLE:
        return "'" + string.replace(/'/g, "''") + "'";
      case STYLE_LITERAL:
        return '|' + blockHeader(string, state.indent)
          + dropEndingNewline(indentString(string, indent));
      case STYLE_FOLDED:
        return '>' + blockHeader(string, state.indent)
          + dropEndingNewline(indentString(foldString(string, lineWidth), indent));
      case STYLE_DOUBLE:
        return '"' + escapeString(string) + '"';
      default:
        throw new exception('impossible error: invalid scalar style');
    }
  }());
}

// Pre-conditions: string is valid for a block scalar, 1 <= indentPerLevel <= 9.
function blockHeader(string, indentPerLevel) {
  var indentIndicator = needIndentIndicator(string) ? String(indentPerLevel) : '';

  // note the special case: the string '\n' counts as a "trailing" empty line.
  var clip =          string[string.length - 1] === '\n';
  var keep = clip && (string[string.length - 2] === '\n' || string === '\n');
  var chomp = keep ? '+' : (clip ? '' : '-');

  return indentIndicator + chomp + '\n';
}

// (See the note for writeScalar.)
function dropEndingNewline(string) {
  return string[string.length - 1] === '\n' ? string.slice(0, -1) : string;
}

// Note: a long line without a suitable break point will exceed the width limit.
// Pre-conditions: every char in str isPrintable, str.length > 0, width > 0.
function foldString(string, width) {
  // In folded style, $k$ consecutive newlines output as $k+1$ newlines—
  // unless they're before or after a more-indented line, or at the very
  // beginning or end, in which case $k$ maps to $k$.
  // Therefore, parse each chunk as newline(s) followed by a content line.
  var lineRe = /(\n+)([^\n]*)/g;

  // first line (possibly an empty line)
  var result = (function () {
    var nextLF = string.indexOf('\n');
    nextLF = nextLF !== -1 ? nextLF : string.length;
    lineRe.lastIndex = nextLF;
    return foldLine(string.slice(0, nextLF), width);
  }());
  // If we haven't reached the first content line yet, don't add an extra \n.
  var prevMoreIndented = string[0] === '\n' || string[0] === ' ';
  var moreIndented;

  // rest of the lines
  var match;
  while ((match = lineRe.exec(string))) {
    var prefix = match[1], line = match[2];
    moreIndented = (line[0] === ' ');
    result += prefix
      + (!prevMoreIndented && !moreIndented && line !== ''
        ? '\n' : '')
      + foldLine(line, width);
    prevMoreIndented = moreIndented;
  }

  return result;
}

// Greedy line breaking.
// Picks the longest line under the limit each time,
// otherwise settles for the shortest line over the limit.
// NB. More-indented lines *cannot* be folded, as that would add an extra \n.
function foldLine(line, width) {
  if (line === '' || line[0] === ' ') return line;

  // Since a more-indented line adds a \n, breaks can't be followed by a space.
  var breakRe = / [^ ]/g; // note: the match index will always be <= length-2.
  var match;
  // start is an inclusive index. end, curr, and next are exclusive.
  var start = 0, end, curr = 0, next = 0;
  var result = '';

  // Invariants: 0 <= start <= length-1.
  //   0 <= curr <= next <= max(0, length-2). curr - start <= width.
  // Inside the loop:
  //   A match implies length >= 2, so curr and next are <= length-2.
  while ((match = breakRe.exec(line))) {
    next = match.index;
    // maintain invariant: curr - start <= width
    if (next - start > width) {
      end = (curr > start) ? curr : next; // derive end <= length-2
      result += '\n' + line.slice(start, end);
      // skip the space that was output as \n
      start = end + 1;                    // derive start <= length-1
    }
    curr = next;
  }

  // By the invariants, start <= length-1, so there is something left over.
  // It is either the whole string or a part starting from non-whitespace.
  result += '\n';
  // Insert a break if the remainder is too long and there is a break available.
  if (line.length - start > width && curr > start) {
    result += line.slice(start, curr) + '\n' + line.slice(curr + 1);
  } else {
    result += line.slice(start);
  }

  return result.slice(1); // drop extra \n joiner
}

// Escapes a double-quoted string.
function escapeString(string) {
  var result = '';
  var char = 0;
  var escapeSeq;

  for (var i = 0; i < string.length; char >= 0x10000 ? i += 2 : i++) {
    char = codePointAt(string, i);
    escapeSeq = ESCAPE_SEQUENCES[char];

    if (!escapeSeq && isPrintable(char)) {
      result += string[i];
      if (char >= 0x10000) result += string[i + 1];
    } else {
      result += escapeSeq || encodeHex(char);
    }
  }

  return result;
}

function writeFlowSequence(state, level, object) {
  var _result = '',
      _tag    = state.tag,
      index,
      length,
      value;

  for (index = 0, length = object.length; index < length; index += 1) {
    value = object[index];

    if (state.replacer) {
      value = state.replacer.call(object, String(index), value);
    }

    // Write only valid elements, put null instead of invalid elements.
    if (writeNode(state, level, value, false, false) ||
        (typeof value === 'undefined' &&
         writeNode(state, level, null, false, false))) {

      if (_result !== '') _result += ',' + (!state.condenseFlow ? ' ' : '');
      _result += state.dump;
    }
  }

  state.tag = _tag;
  state.dump = '[' + _result + ']';
}

function writeBlockSequence(state, level, object, compact) {
  var _result = '',
      _tag    = state.tag,
      index,
      length,
      value;

  for (index = 0, length = object.length; index < length; index += 1) {
    value = object[index];

    if (state.replacer) {
      value = state.replacer.call(object, String(index), value);
    }

    // Write only valid elements, put null instead of invalid elements.
    if (writeNode(state, level + 1, value, true, true, false, true) ||
        (typeof value === 'undefined' &&
         writeNode(state, level + 1, null, true, true, false, true))) {

      if (!compact || _result !== '') {
        _result += generateNextLine(state, level);
      }

      if (state.dump && CHAR_LINE_FEED === state.dump.charCodeAt(0)) {
        _result += '-';
      } else {
        _result += '- ';
      }

      _result += state.dump;
    }
  }

  state.tag = _tag;
  state.dump = _result || '[]'; // Empty sequence if no valid values.
}

function writeFlowMapping(state, level, object) {
  var _result       = '',
      _tag          = state.tag,
      objectKeyList = Object.keys(object),
      index,
      length,
      objectKey,
      objectValue,
      pairBuffer;

  for (index = 0, length = objectKeyList.length; index < length; index += 1) {

    pairBuffer = '';
    if (_result !== '') pairBuffer += ', ';

    if (state.condenseFlow) pairBuffer += '"';

    objectKey = objectKeyList[index];
    objectValue = object[objectKey];

    if (state.replacer) {
      objectValue = state.replacer.call(object, objectKey, objectValue);
    }

    if (!writeNode(state, level, objectKey, false, false)) {
      continue; // Skip this pair because of invalid key;
    }

    if (state.dump.length > 1024) pairBuffer += '? ';

    pairBuffer += state.dump + (state.condenseFlow ? '"' : '') + ':' + (state.condenseFlow ? '' : ' ');

    if (!writeNode(state, level, objectValue, false, false)) {
      continue; // Skip this pair because of invalid value.
    }

    pairBuffer += state.dump;

    // Both key and value are valid.
    _result += pairBuffer;
  }

  state.tag = _tag;
  state.dump = '{' + _result + '}';
}

function writeBlockMapping(state, level, object, compact) {
  var _result       = '',
      _tag          = state.tag,
      objectKeyList = Object.keys(object),
      index,
      length,
      objectKey,
      objectValue,
      explicitPair,
      pairBuffer;

  // Allow sorting keys so that the output file is deterministic
  if (state.sortKeys === true) {
    // Default sorting
    objectKeyList.sort();
  } else if (typeof state.sortKeys === 'function') {
    // Custom sort function
    objectKeyList.sort(state.sortKeys);
  } else if (state.sortKeys) {
    // Something is wrong
    throw new exception('sortKeys must be a boolean or a function');
  }

  for (index = 0, length = objectKeyList.length; index < length; index += 1) {
    pairBuffer = '';

    if (!compact || _result !== '') {
      pairBuffer += generateNextLine(state, level);
    }

    objectKey = objectKeyList[index];
    objectValue = object[objectKey];

    if (state.replacer) {
      objectValue = state.replacer.call(object, objectKey, objectValue);
    }

    if (!writeNode(state, level + 1, objectKey, true, true, true)) {
      continue; // Skip this pair because of invalid key.
    }

    explicitPair = (state.tag !== null && state.tag !== '?') ||
                   (state.dump && state.dump.length > 1024);

    if (explicitPair) {
      if (state.dump && CHAR_LINE_FEED === state.dump.charCodeAt(0)) {
        pairBuffer += '?';
      } else {
        pairBuffer += '? ';
      }
    }

    pairBuffer += state.dump;

    if (explicitPair) {
      pairBuffer += generateNextLine(state, level);
    }

    if (!writeNode(state, level + 1, objectValue, true, explicitPair)) {
      continue; // Skip this pair because of invalid value.
    }

    if (state.dump && CHAR_LINE_FEED === state.dump.charCodeAt(0)) {
      pairBuffer += ':';
    } else {
      pairBuffer += ': ';
    }

    pairBuffer += state.dump;

    // Both key and value are valid.
    _result += pairBuffer;
  }

  state.tag = _tag;
  state.dump = _result || '{}'; // Empty mapping if no valid pairs.
}

function detectType(state, object, explicit) {
  var _result, typeList, index, length, type, style;

  typeList = explicit ? state.explicitTypes : state.implicitTypes;

  for (index = 0, length = typeList.length; index < length; index += 1) {
    type = typeList[index];

    if ((type.instanceOf  || type.predicate) &&
        (!type.instanceOf || ((typeof object === 'object') && (object instanceof type.instanceOf))) &&
        (!type.predicate  || type.predicate(object))) {

      if (explicit) {
        if (type.multi && type.representName) {
          state.tag = type.representName(object);
        } else {
          state.tag = type.tag;
        }
      } else {
        state.tag = '?';
      }

      if (type.represent) {
        style = state.styleMap[type.tag] || type.defaultStyle;

        if (_toString.call(type.represent) === '[object Function]') {
          _result = type.represent(object, style);
        } else if (_hasOwnProperty.call(type.represent, style)) {
          _result = type.represent[style](object, style);
        } else {
          throw new exception('!<' + type.tag + '> tag resolver accepts not "' + style + '" style');
        }

        state.dump = _result;
      }

      return true;
    }
  }

  return false;
}

// Serializes `object` and writes it to global `result`.
// Returns true on success, or false on invalid object.
//
function writeNode(state, level, object, block, compact, iskey, isblockseq) {
  state.tag = null;
  state.dump = object;

  if (!detectType(state, object, false)) {
    detectType(state, object, true);
  }

  var type = _toString.call(state.dump);
  var inblock = block;
  var tagStr;

  if (block) {
    block = (state.flowLevel < 0 || state.flowLevel > level);
  }

  var objectOrArray = type === '[object Object]' || type === '[object Array]',
      duplicateIndex,
      duplicate;

  if (objectOrArray) {
    duplicateIndex = state.duplicates.indexOf(object);
    duplicate = duplicateIndex !== -1;
  }

  if ((state.tag !== null && state.tag !== '?') || duplicate || (state.indent !== 2 && level > 0)) {
    compact = false;
  }

  if (duplicate && state.usedDuplicates[duplicateIndex]) {
    state.dump = '*ref_' + duplicateIndex;
  } else {
    if (objectOrArray && duplicate && !state.usedDuplicates[duplicateIndex]) {
      state.usedDuplicates[duplicateIndex] = true;
    }
    if (type === '[object Object]') {
      if (block && (Object.keys(state.dump).length !== 0)) {
        writeBlockMapping(state, level, state.dump, compact);
        if (duplicate) {
          state.dump = '&ref_' + duplicateIndex + state.dump;
        }
      } else {
        writeFlowMapping(state, level, state.dump);
        if (duplicate) {
          state.dump = '&ref_' + duplicateIndex + ' ' + state.dump;
        }
      }
    } else if (type === '[object Array]') {
      if (block && (state.dump.length !== 0)) {
        if (state.noArrayIndent && !isblockseq && level > 0) {
          writeBlockSequence(state, level - 1, state.dump, compact);
        } else {
          writeBlockSequence(state, level, state.dump, compact);
        }
        if (duplicate) {
          state.dump = '&ref_' + duplicateIndex + state.dump;
        }
      } else {
        writeFlowSequence(state, level, state.dump);
        if (duplicate) {
          state.dump = '&ref_' + duplicateIndex + ' ' + state.dump;
        }
      }
    } else if (type === '[object String]') {
      if (state.tag !== '?') {
        writeScalar(state, state.dump, level, iskey, inblock);
      }
    } else if (type === '[object Undefined]') {
      return false;
    } else {
      if (state.skipInvalid) return false;
      throw new exception('unacceptable kind of an object to dump ' + type);
    }

    if (state.tag !== null && state.tag !== '?') {
      // Need to encode all characters except those allowed by the spec:
      //
      // [35] ns-dec-digit    ::=  [#x30-#x39] /* 0-9 */
      // [36] ns-hex-digit    ::=  ns-dec-digit
      //                         | [#x41-#x46] /* A-F */ | [#x61-#x66] /* a-f */
      // [37] ns-ascii-letter ::=  [#x41-#x5A] /* A-Z */ | [#x61-#x7A] /* a-z */
      // [38] ns-word-char    ::=  ns-dec-digit | ns-ascii-letter | “-”
      // [39] ns-uri-char     ::=  “%” ns-hex-digit ns-hex-digit | ns-word-char | “#”
      //                         | “;” | “/” | “?” | “:” | “@” | “&” | “=” | “+” | “$” | “,”
      //                         | “_” | “.” | “!” | “~” | “*” | “'” | “(” | “)” | “[” | “]”
      //
      // Also need to encode '!' because it has special meaning (end of tag prefix).
      //
      tagStr = encodeURI(
        state.tag[0] === '!' ? state.tag.slice(1) : state.tag
      ).replace(/!/g, '%21');

      if (state.tag[0] === '!') {
        tagStr = '!' + tagStr;
      } else if (tagStr.slice(0, 18) === 'tag:yaml.org,2002:') {
        tagStr = '!!' + tagStr.slice(18);
      } else {
        tagStr = '!<' + tagStr + '>';
      }

      state.dump = tagStr + ' ' + state.dump;
    }
  }

  return true;
}

function getDuplicateReferences(object, state) {
  var objects = [],
      duplicatesIndexes = [],
      index,
      length;

  inspectNode(object, objects, duplicatesIndexes);

  for (index = 0, length = duplicatesIndexes.length; index < length; index += 1) {
    state.duplicates.push(objects[duplicatesIndexes[index]]);
  }
  state.usedDuplicates = new Array(length);
}

function inspectNode(object, objects, duplicatesIndexes) {
  var objectKeyList,
      index,
      length;

  if (object !== null && typeof object === 'object') {
    index = objects.indexOf(object);
    if (index !== -1) {
      if (duplicatesIndexes.indexOf(index) === -1) {
        duplicatesIndexes.push(index);
      }
    } else {
      objects.push(object);

      if (Array.isArray(object)) {
        for (index = 0, length = object.length; index < length; index += 1) {
          inspectNode(object[index], objects, duplicatesIndexes);
        }
      } else {
        objectKeyList = Object.keys(object);

        for (index = 0, length = objectKeyList.length; index < length; index += 1) {
          inspectNode(object[objectKeyList[index]], objects, duplicatesIndexes);
        }
      }
    }
  }
}

function dump$1(input, options) {
  options = options || {};

  var state = new State(options);

  if (!state.noRefs) getDuplicateReferences(input, state);

  var value = input;

  if (state.replacer) {
    value = state.replacer.call({ '': value }, '', value);
  }

  if (writeNode(state, 0, value, true, true)) return state.dump + '\n';

  return '';
}

var dump_1 = dump$1;

var dumper = {
	dump: dump_1
};

function renamed(from, to) {
  return function () {
    throw new Error('Function yaml.' + from + ' is removed in js-yaml 4. ' +
      'Use yaml.' + to + ' instead, which is now safe by default.');
  };
}


var Type                = type;
var Schema              = schema;
var FAILSAFE_SCHEMA     = failsafe;
var JSON_SCHEMA         = json;
var CORE_SCHEMA         = core;
var DEFAULT_SCHEMA      = _default;
var load                = loader.load;
var loadAll             = loader.loadAll;
var dump                = dumper.dump;
var YAMLException       = exception;

// Re-export all types in case user wants to create custom schema
var types = {
  binary:    binary,
  float:     float,
  map:       map,
  null:      _null,
  pairs:     pairs,
  set:       set,
  timestamp: timestamp,
  bool:      bool,
  int:       int,
  merge:     merge,
  omap:      omap,
  seq:       seq,
  str:       str
};

// Removed functions from JS-YAML 3.0.x
var safeLoad            = renamed('safeLoad', 'load');
var safeLoadAll         = renamed('safeLoadAll', 'loadAll');
var safeDump            = renamed('safeDump', 'dump');

var jsYaml = {
	Type: Type,
	Schema: Schema,
	FAILSAFE_SCHEMA: FAILSAFE_SCHEMA,
	JSON_SCHEMA: JSON_SCHEMA,
	CORE_SCHEMA: CORE_SCHEMA,
	DEFAULT_SCHEMA: DEFAULT_SCHEMA,
	load: load,
	loadAll: loadAll,
	dump: dump,
	YAMLException: YAMLException,
	types: types,
	safeLoad: safeLoad,
	safeLoadAll: safeLoadAll,
	safeDump: safeDump
};

/* ESM default export */ const __WEBPACK_DEFAULT_EXPORT__ = (jsYaml);



}),
"./node_modules/.pnpm/lit-element@4.2.1/node_modules/lit-element/development/lit-element.js": 
/*!**************************************************************************************************!*\
  !*** ./node_modules/.pnpm/lit-element@4.2.1/node_modules/lit-element/development/lit-element.js ***!
  \**************************************************************************************************/
(function (__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  CSSResult: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.CSSResult),
  LitElement: () => (LitElement),
  ReactiveElement: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.ReactiveElement),
  _$LE: () => (_$LE),
  _$LH: () => (/* reexport safe */ lit_html__WEBPACK_IMPORTED_MODULE_1__._$LH),
  adoptStyles: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.adoptStyles),
  css: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.css),
  defaultConverter: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.defaultConverter),
  getCompatibleStyle: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.getCompatibleStyle),
  html: () => (/* reexport safe */ lit_html__WEBPACK_IMPORTED_MODULE_1__.html),
  mathml: () => (/* reexport safe */ lit_html__WEBPACK_IMPORTED_MODULE_1__.mathml),
  noChange: () => (/* reexport safe */ lit_html__WEBPACK_IMPORTED_MODULE_1__.noChange),
  notEqual: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.notEqual),
  nothing: () => (/* reexport safe */ lit_html__WEBPACK_IMPORTED_MODULE_1__.nothing),
  render: () => (/* reexport safe */ lit_html__WEBPACK_IMPORTED_MODULE_1__.render),
  supportsAdoptingStyleSheets: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.supportsAdoptingStyleSheets),
  svg: () => (/* reexport safe */ lit_html__WEBPACK_IMPORTED_MODULE_1__.svg),
  unsafeCSS: () => (/* reexport safe */ _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.unsafeCSS)
});
/* ESM import */var _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @lit/reactive-element */ "./node_modules/.pnpm/@lit+reactive-element@2.1.1/node_modules/@lit/reactive-element/development/reactive-element.js");
/* ESM import */var lit_html__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! lit-html */ "./node_modules/.pnpm/lit-html@3.3.1/node_modules/lit-html/development/lit-html.js");
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
/**
 * The main LitElement module, which defines the {@linkcode LitElement} base
 * class and related APIs.
 *
 * LitElement components can define a template and a set of observed
 * properties. Changing an observed property triggers a re-render of the
 * element.
 *
 * Import {@linkcode LitElement} and {@linkcode html} from this module to
 * create a component:
 *
 *  ```js
 * import {LitElement, html} from 'lit-element';
 *
 * class MyElement extends LitElement {
 *
 *   // Declare observed properties
 *   static get properties() {
 *     return {
 *       adjective: {}
 *     }
 *   }
 *
 *   constructor() {
 *     this.adjective = 'awesome';
 *   }
 *
 *   // Define the element's template
 *   render() {
 *     return html`<p>your ${adjective} template here</p>`;
 *   }
 * }
 *
 * customElements.define('my-element', MyElement);
 * ```
 *
 * `LitElement` extends {@linkcode ReactiveElement} and adds lit-html
 * templating. The `ReactiveElement` class is provided for users that want to
 * build their own custom element base classes that don't use lit-html.
 *
 * @packageDocumentation
 */




/*
 * When using Closure Compiler, JSCompiler_renameProperty(property, object) is
 * replaced at compile time by the munged name for object[property]. We cannot
 * alias this function, so we have to use a small shim that has the same
 * behavior when not compiling.
 */
/*@__INLINE__*/
const JSCompiler_renameProperty = (prop, _obj) => prop;
const DEV_MODE = true;
// Allows minifiers to rename references to globalThis
const global = globalThis;
let issueWarning;
if (DEV_MODE) {
    // Ensure warnings are issued only 1x, even if multiple versions of Lit
    // are loaded.
    global.litIssuedWarnings ??= new Set();
    /**
     * Issue a warning if we haven't already, based either on `code` or `warning`.
     * Warnings are disabled automatically only by `warning`; disabling via `code`
     * can be done by users.
     */
    issueWarning = (code, warning) => {
        warning += ` See https://lit.dev/msg/${code} for more information.`;
        if (!global.litIssuedWarnings.has(warning) &&
            !global.litIssuedWarnings.has(code)) {
            console.warn(warning);
            global.litIssuedWarnings.add(warning);
        }
    };
}
/**
 * Base element class that manages element properties and attributes, and
 * renders a lit-html template.
 *
 * To define a component, subclass `LitElement` and implement a
 * `render` method to provide the component's template. Define properties
 * using the {@linkcode LitElement.properties properties} property or the
 * {@linkcode property} decorator.
 */
class LitElement extends _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__.ReactiveElement {
    constructor() {
        super(...arguments);
        /**
         * @category rendering
         */
        this.renderOptions = { host: this };
        this.__childPart = undefined;
    }
    /**
     * @category rendering
     */
    createRenderRoot() {
        const renderRoot = super.createRenderRoot();
        // When adoptedStyleSheets are shimmed, they are inserted into the
        // shadowRoot by createRenderRoot. Adjust the renderBefore node so that
        // any styles in Lit content render before adoptedStyleSheets. This is
        // important so that adoptedStyleSheets have precedence over styles in
        // the shadowRoot.
        this.renderOptions.renderBefore ??= renderRoot.firstChild;
        return renderRoot;
    }
    /**
     * Updates the element. This method reflects property values to attributes
     * and calls `render` to render DOM via lit-html. Setting properties inside
     * this method will *not* trigger another update.
     * @param changedProperties Map of changed properties with old values
     * @category updates
     */
    update(changedProperties) {
        // Setting properties in `render` should not trigger an update. Since
        // updates are allowed after super.update, it's important to call `render`
        // before that.
        const value = this.render();
        if (!this.hasUpdated) {
            this.renderOptions.isConnected = this.isConnected;
        }
        super.update(changedProperties);
        this.__childPart = (0,lit_html__WEBPACK_IMPORTED_MODULE_1__.render)(value, this.renderRoot, this.renderOptions);
    }
    /**
     * Invoked when the component is added to the document's DOM.
     *
     * In `connectedCallback()` you should setup tasks that should only occur when
     * the element is connected to the document. The most common of these is
     * adding event listeners to nodes external to the element, like a keydown
     * event handler added to the window.
     *
     * ```ts
     * connectedCallback() {
     *   super.connectedCallback();
     *   addEventListener('keydown', this._handleKeydown);
     * }
     * ```
     *
     * Typically, anything done in `connectedCallback()` should be undone when the
     * element is disconnected, in `disconnectedCallback()`.
     *
     * @category lifecycle
     */
    connectedCallback() {
        super.connectedCallback();
        this.__childPart?.setConnected(true);
    }
    /**
     * Invoked when the component is removed from the document's DOM.
     *
     * This callback is the main signal to the element that it may no longer be
     * used. `disconnectedCallback()` should ensure that nothing is holding a
     * reference to the element (such as event listeners added to nodes external
     * to the element), so that it is free to be garbage collected.
     *
     * ```ts
     * disconnectedCallback() {
     *   super.disconnectedCallback();
     *   window.removeEventListener('keydown', this._handleKeydown);
     * }
     * ```
     *
     * An element may be re-connected after being disconnected.
     *
     * @category lifecycle
     */
    disconnectedCallback() {
        super.disconnectedCallback();
        this.__childPart?.setConnected(false);
    }
    /**
     * Invoked on each update to perform rendering tasks. This method may return
     * any value renderable by lit-html's `ChildPart` - typically a
     * `TemplateResult`. Setting properties inside this method will *not* trigger
     * the element to update.
     * @category rendering
     */
    render() {
        return lit_html__WEBPACK_IMPORTED_MODULE_1__.noChange;
    }
}
// This property needs to remain unminified.
LitElement['_$litElement$'] = true;
/**
 * Ensure this class is marked as `finalized` as an optimization ensuring
 * it will not needlessly try to `finalize`.
 *
 * Note this property name is a string to prevent breaking Closure JS Compiler
 * optimizations. See @lit/reactive-element for more information.
 */
LitElement[JSCompiler_renameProperty('finalized', LitElement)] = true;
// Install hydration if available
global.litElementHydrateSupport?.({ LitElement });
// Apply polyfills if available
const polyfillSupport = DEV_MODE
    ? global.litElementPolyfillSupportDevMode
    : global.litElementPolyfillSupport;
polyfillSupport?.({ LitElement });
/**
 * END USERS SHOULD NOT RELY ON THIS OBJECT.
 *
 * Private exports for use by other Lit packages, not intended for use by
 * external users.
 *
 * We currently do not make a mangled rollup build of the lit-ssr code. In order
 * to keep a number of (otherwise private) top-level exports  mangled in the
 * client side code, we export a _$LE object containing those members (or
 * helper methods for accessing private fields of those members), and then
 * re-export them for use in lit-ssr. This keeps lit-ssr agnostic to whether the
 * client-side code is being used in `dev` mode or `prod` mode.
 *
 * This has a unique name, to disambiguate it from private exports in
 * lit-html, since this module re-exports all of lit-html.
 *
 * @private
 */
const _$LE = {
    _$attributeToProperty: (el, name, value) => {
        // eslint-disable-next-line
        el._$attributeToProperty(name, value);
    },
    // eslint-disable-next-line
    _$changedProperties: (el) => el._$changedProperties,
};
// IMPORTANT: do not change the property name or the assignment expression.
// This line will be used in regexes to search for LitElement usage.
(global.litElementVersions ??= []).push('4.2.1');
if (DEV_MODE && global.litElementVersions.length > 1) {
    queueMicrotask(() => {
        issueWarning('multiple-versions', `Multiple versions of Lit loaded. Loading multiple versions ` +
            `is not recommended.`);
    });
}
//# sourceMappingURL=lit-element.js.map

}),
"./node_modules/.pnpm/lit-html@3.3.1/node_modules/lit-html/development/is-server.js": 
/*!******************************************************************************************!*\
  !*** ./node_modules/.pnpm/lit-html@3.3.1/node_modules/lit-html/development/is-server.js ***!
  \******************************************************************************************/
(function (__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  isServer: () => (isServer)
});
/**
 * @license
 * Copyright 2022 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
/**
 * @fileoverview
 *
 * This file exports a boolean const whose value will depend on what environment
 * the module is being imported from.
 */
const NODE_MODE = false;
/**
 * A boolean that will be `true` in server environments like Node, and `false`
 * in browser environments. Note that your server environment or toolchain must
 * support the `"node"` export condition for this to be `true`.
 *
 * This can be used when authoring components to change behavior based on
 * whether or not the component is executing in an SSR context.
 */
const isServer = NODE_MODE;
//# sourceMappingURL=is-server.js.map

}),
"./node_modules/.pnpm/lit-html@3.3.1/node_modules/lit-html/development/lit-html.js": 
/*!*****************************************************************************************!*\
  !*** ./node_modules/.pnpm/lit-html@3.3.1/node_modules/lit-html/development/lit-html.js ***!
  \*****************************************************************************************/
(function (__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  _$LH: () => (_$LH),
  html: () => (html),
  mathml: () => (mathml),
  noChange: () => (noChange),
  nothing: () => (nothing),
  render: () => (render),
  svg: () => (svg)
});
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
const DEV_MODE = true;
const ENABLE_EXTRA_SECURITY_HOOKS = true;
const ENABLE_SHADYDOM_NOPATCH = true;
const NODE_MODE = false;
// Allows minifiers to rename references to globalThis
const global = globalThis;
/**
 * Useful for visualizing and logging insights into what the Lit template system is doing.
 *
 * Compiled out of prod mode builds.
 */
const debugLogEvent = DEV_MODE
    ? (event) => {
        const shouldEmit = global
            .emitLitDebugLogEvents;
        if (!shouldEmit) {
            return;
        }
        global.dispatchEvent(new CustomEvent('lit-debug', {
            detail: event,
        }));
    }
    : undefined;
// Used for connecting beginRender and endRender events when there are nested
// renders when errors are thrown preventing an endRender event from being
// called.
let debugLogRenderId = 0;
let issueWarning;
if (DEV_MODE) {
    global.litIssuedWarnings ??= new Set();
    /**
     * Issue a warning if we haven't already, based either on `code` or `warning`.
     * Warnings are disabled automatically only by `warning`; disabling via `code`
     * can be done by users.
     */
    issueWarning = (code, warning) => {
        warning += code
            ? ` See https://lit.dev/msg/${code} for more information.`
            : '';
        if (!global.litIssuedWarnings.has(warning) &&
            !global.litIssuedWarnings.has(code)) {
            console.warn(warning);
            global.litIssuedWarnings.add(warning);
        }
    };
    queueMicrotask(() => {
        issueWarning('dev-mode', `Lit is in dev mode. Not recommended for production!`);
    });
}
const wrap = ENABLE_SHADYDOM_NOPATCH &&
    global.ShadyDOM?.inUse &&
    global.ShadyDOM?.noPatch === true
    ? global.ShadyDOM.wrap
    : (node) => node;
const trustedTypes = global.trustedTypes;
/**
 * Our TrustedTypePolicy for HTML which is declared using the html template
 * tag function.
 *
 * That HTML is a developer-authored constant, and is parsed with innerHTML
 * before any untrusted expressions have been mixed in. Therefor it is
 * considered safe by construction.
 */
const policy = trustedTypes
    ? trustedTypes.createPolicy('lit-html', {
        createHTML: (s) => s,
    })
    : undefined;
const identityFunction = (value) => value;
const noopSanitizer = (_node, _name, _type) => identityFunction;
/** Sets the global sanitizer factory. */
const setSanitizer = (newSanitizer) => {
    if (!ENABLE_EXTRA_SECURITY_HOOKS) {
        return;
    }
    if (sanitizerFactoryInternal !== noopSanitizer) {
        throw new Error(`Attempted to overwrite existing lit-html security policy.` +
            ` setSanitizeDOMValueFactory should be called at most once.`);
    }
    sanitizerFactoryInternal = newSanitizer;
};
/**
 * Only used in internal tests, not a part of the public API.
 */
const _testOnlyClearSanitizerFactoryDoNotCallOrElse = () => {
    sanitizerFactoryInternal = noopSanitizer;
};
const createSanitizer = (node, name, type) => {
    return sanitizerFactoryInternal(node, name, type);
};
// Added to an attribute name to mark the attribute as bound so we can find
// it easily.
const boundAttributeSuffix = '$lit$';
// This marker is used in many syntactic positions in HTML, so it must be
// a valid element name and attribute name. We don't support dynamic names (yet)
// but this at least ensures that the parse tree is closer to the template
// intention.
const marker = `lit$${Math.random().toFixed(9).slice(2)}$`;
// String used to tell if a comment is a marker comment
const markerMatch = '?' + marker;
// Text used to insert a comment marker node. We use processing instruction
// syntax because it's slightly smaller, but parses as a comment node.
const nodeMarker = `<${markerMatch}>`;
const d = NODE_MODE && global.document === undefined
    ? {
        createTreeWalker() {
            return {};
        },
    }
    : document;
// Creates a dynamic marker. We never have to search for these in the DOM.
const createMarker = () => d.createComment('');
const isPrimitive = (value) => value === null || (typeof value != 'object' && typeof value != 'function');
const isArray = Array.isArray;
const isIterable = (value) => isArray(value) ||
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    typeof value?.[Symbol.iterator] === 'function';
const SPACE_CHAR = `[ \t\n\f\r]`;
const ATTR_VALUE_CHAR = `[^ \t\n\f\r"'\`<>=]`;
const NAME_CHAR = `[^\\s"'>=/]`;
// These regexes represent the five parsing states that we care about in the
// Template's HTML scanner. They match the *end* of the state they're named
// after.
// Depending on the match, we transition to a new state. If there's no match,
// we stay in the same state.
// Note that the regexes are stateful. We utilize lastIndex and sync it
// across the multiple regexes used. In addition to the five regexes below
// we also dynamically create a regex to find the matching end tags for raw
// text elements.
/**
 * End of text is: `<` followed by:
 *   (comment start) or (tag) or (dynamic tag binding)
 */
const textEndRegex = /<(?:(!--|\/[^a-zA-Z])|(\/?[a-zA-Z][^>\s]*)|(\/?$))/g;
const COMMENT_START = 1;
const TAG_NAME = 2;
const DYNAMIC_TAG_NAME = 3;
const commentEndRegex = /-->/g;
/**
 * Comments not started with <!--, like </{, can be ended by a single `>`
 */
const comment2EndRegex = />/g;
/**
 * The tagEnd regex matches the end of the "inside an opening" tag syntax
 * position. It either matches a `>`, an attribute-like sequence, or the end
 * of the string after a space (attribute-name position ending).
 *
 * See attributes in the HTML spec:
 * https://www.w3.org/TR/html5/syntax.html#elements-attributes
 *
 * " \t\n\f\r" are HTML space characters:
 * https://infra.spec.whatwg.org/#ascii-whitespace
 *
 * So an attribute is:
 *  * The name: any character except a whitespace character, ("), ('), ">",
 *    "=", or "/". Note: this is different from the HTML spec which also excludes control characters.
 *  * Followed by zero or more space characters
 *  * Followed by "="
 *  * Followed by zero or more space characters
 *  * Followed by:
 *    * Any character except space, ('), ("), "<", ">", "=", (`), or
 *    * (") then any non-("), or
 *    * (') then any non-(')
 */
const tagEndRegex = new RegExp(`>|${SPACE_CHAR}(?:(${NAME_CHAR}+)(${SPACE_CHAR}*=${SPACE_CHAR}*(?:${ATTR_VALUE_CHAR}|("|')|))|$)`, 'g');
const ENTIRE_MATCH = 0;
const ATTRIBUTE_NAME = 1;
const SPACES_AND_EQUALS = 2;
const QUOTE_CHAR = 3;
const singleQuoteAttrEndRegex = /'/g;
const doubleQuoteAttrEndRegex = /"/g;
/**
 * Matches the raw text elements.
 *
 * Comments are not parsed within raw text elements, so we need to search their
 * text content for marker strings.
 */
const rawTextElement = /^(?:script|style|textarea|title)$/i;
/** TemplateResult types */
const HTML_RESULT = 1;
const SVG_RESULT = 2;
const MATHML_RESULT = 3;
// TemplatePart types
// IMPORTANT: these must match the values in PartType
const ATTRIBUTE_PART = 1;
const CHILD_PART = 2;
const PROPERTY_PART = 3;
const BOOLEAN_ATTRIBUTE_PART = 4;
const EVENT_PART = 5;
const ELEMENT_PART = 6;
const COMMENT_PART = 7;
/**
 * Generates a template literal tag function that returns a TemplateResult with
 * the given result type.
 */
const tag = (type) => (strings, ...values) => {
    // Warn against templates octal escape sequences
    // We do this here rather than in render so that the warning is closer to the
    // template definition.
    if (DEV_MODE && strings.some((s) => s === undefined)) {
        console.warn('Some template strings are undefined.\n' +
            'This is probably caused by illegal octal escape sequences.');
    }
    if (DEV_MODE) {
        // Import static-html.js results in a circular dependency which g3 doesn't
        // handle. Instead we know that static values must have the field
        // `_$litStatic$`.
        if (values.some((val) => val?.['_$litStatic$'])) {
            issueWarning('', `Static values 'literal' or 'unsafeStatic' cannot be used as values to non-static templates.\n` +
                `Please use the static 'html' tag function. See https://lit.dev/docs/templates/expressions/#static-expressions`);
        }
    }
    return {
        // This property needs to remain unminified.
        ['_$litType$']: type,
        strings,
        values,
    };
};
/**
 * Interprets a template literal as an HTML template that can efficiently
 * render to and update a container.
 *
 * ```ts
 * const header = (title: string) => html`<h1>${title}</h1>`;
 * ```
 *
 * The `html` tag returns a description of the DOM to render as a value. It is
 * lazy, meaning no work is done until the template is rendered. When rendering,
 * if a template comes from the same expression as a previously rendered result,
 * it's efficiently updated instead of replaced.
 */
const html = tag(HTML_RESULT);
/**
 * Interprets a template literal as an SVG fragment that can efficiently render
 * to and update a container.
 *
 * ```ts
 * const rect = svg`<rect width="10" height="10"></rect>`;
 *
 * const myImage = html`
 *   <svg viewBox="0 0 10 10" xmlns="http://www.w3.org/2000/svg">
 *     ${rect}
 *   </svg>`;
 * ```
 *
 * The `svg` *tag function* should only be used for SVG fragments, or elements
 * that would be contained **inside** an `<svg>` HTML element. A common error is
 * placing an `<svg>` *element* in a template tagged with the `svg` tag
 * function. The `<svg>` element is an HTML element and should be used within a
 * template tagged with the {@linkcode html} tag function.
 *
 * In LitElement usage, it's invalid to return an SVG fragment from the
 * `render()` method, as the SVG fragment will be contained within the element's
 * shadow root and thus not be properly contained within an `<svg>` HTML
 * element.
 */
const svg = tag(SVG_RESULT);
/**
 * Interprets a template literal as MathML fragment that can efficiently render
 * to and update a container.
 *
 * ```ts
 * const num = mathml`<mn>1</mn>`;
 *
 * const eq = html`
 *   <math>
 *     ${num}
 *   </math>`;
 * ```
 *
 * The `mathml` *tag function* should only be used for MathML fragments, or
 * elements that would be contained **inside** a `<math>` HTML element. A common
 * error is placing a `<math>` *element* in a template tagged with the `mathml`
 * tag function. The `<math>` element is an HTML element and should be used
 * within a template tagged with the {@linkcode html} tag function.
 *
 * In LitElement usage, it's invalid to return an MathML fragment from the
 * `render()` method, as the MathML fragment will be contained within the
 * element's shadow root and thus not be properly contained within a `<math>`
 * HTML element.
 */
const mathml = tag(MATHML_RESULT);
/**
 * A sentinel value that signals that a value was handled by a directive and
 * should not be written to the DOM.
 */
const noChange = Symbol.for('lit-noChange');
/**
 * A sentinel value that signals a ChildPart to fully clear its content.
 *
 * ```ts
 * const button = html`${
 *  user.isAdmin
 *    ? html`<button>DELETE</button>`
 *    : nothing
 * }`;
 * ```
 *
 * Prefer using `nothing` over other falsy values as it provides a consistent
 * behavior between various expression binding contexts.
 *
 * In child expressions, `undefined`, `null`, `''`, and `nothing` all behave the
 * same and render no nodes. In attribute expressions, `nothing` _removes_ the
 * attribute, while `undefined` and `null` will render an empty string. In
 * property expressions `nothing` becomes `undefined`.
 */
const nothing = Symbol.for('lit-nothing');
/**
 * The cache of prepared templates, keyed by the tagged TemplateStringsArray
 * and _not_ accounting for the specific template tag used. This means that
 * template tags cannot be dynamic - they must statically be one of html, svg,
 * or attr. This restriction simplifies the cache lookup, which is on the hot
 * path for rendering.
 */
const templateCache = new WeakMap();
const walker = d.createTreeWalker(d, 129 /* NodeFilter.SHOW_{ELEMENT|COMMENT} */);
let sanitizerFactoryInternal = noopSanitizer;
function trustFromTemplateString(tsa, stringFromTSA) {
    // A security check to prevent spoofing of Lit template results.
    // In the future, we may be able to replace this with Array.isTemplateObject,
    // though we might need to make that check inside of the html and svg
    // functions, because precompiled templates don't come in as
    // TemplateStringArray objects.
    if (!isArray(tsa) || !tsa.hasOwnProperty('raw')) {
        let message = 'invalid template strings array';
        if (DEV_MODE) {
            message = `
          Internal Error: expected template strings to be an array
          with a 'raw' field. Faking a template strings array by
          calling html or svg like an ordinary function is effectively
          the same as calling unsafeHtml and can lead to major security
          issues, e.g. opening your code up to XSS attacks.
          If you're using the html or svg tagged template functions normally
          and still seeing this error, please file a bug at
          https://github.com/lit/lit/issues/new?template=bug_report.md
          and include information about your build tooling, if any.
        `
                .trim()
                .replace(/\n */g, '\n');
        }
        throw new Error(message);
    }
    return policy !== undefined
        ? policy.createHTML(stringFromTSA)
        : stringFromTSA;
}
/**
 * Returns an HTML string for the given TemplateStringsArray and result type
 * (HTML or SVG), along with the case-sensitive bound attribute names in
 * template order. The HTML contains comment markers denoting the `ChildPart`s
 * and suffixes on bound attributes denoting the `AttributeParts`.
 *
 * @param strings template strings array
 * @param type HTML or SVG
 * @return Array containing `[html, attrNames]` (array returned for terseness,
 *     to avoid object fields since this code is shared with non-minified SSR
 *     code)
 */
const getTemplateHtml = (strings, type) => {
    // Insert makers into the template HTML to represent the position of
    // bindings. The following code scans the template strings to determine the
    // syntactic position of the bindings. They can be in text position, where
    // we insert an HTML comment, attribute value position, where we insert a
    // sentinel string and re-write the attribute name, or inside a tag where
    // we insert the sentinel string.
    const l = strings.length - 1;
    // Stores the case-sensitive bound attribute names in the order of their
    // parts. ElementParts are also reflected in this array as undefined
    // rather than a string, to disambiguate from attribute bindings.
    const attrNames = [];
    let html = type === SVG_RESULT ? '<svg>' : type === MATHML_RESULT ? '<math>' : '';
    // When we're inside a raw text tag (not it's text content), the regex
    // will still be tagRegex so we can find attributes, but will switch to
    // this regex when the tag ends.
    let rawTextEndRegex;
    // The current parsing state, represented as a reference to one of the
    // regexes
    let regex = textEndRegex;
    for (let i = 0; i < l; i++) {
        const s = strings[i];
        // The index of the end of the last attribute name. When this is
        // positive at end of a string, it means we're in an attribute value
        // position and need to rewrite the attribute name.
        // We also use a special value of -2 to indicate that we encountered
        // the end of a string in attribute name position.
        let attrNameEndIndex = -1;
        let attrName;
        let lastIndex = 0;
        let match;
        // The conditions in this loop handle the current parse state, and the
        // assignments to the `regex` variable are the state transitions.
        while (lastIndex < s.length) {
            // Make sure we start searching from where we previously left off
            regex.lastIndex = lastIndex;
            match = regex.exec(s);
            if (match === null) {
                break;
            }
            lastIndex = regex.lastIndex;
            if (regex === textEndRegex) {
                if (match[COMMENT_START] === '!--') {
                    regex = commentEndRegex;
                }
                else if (match[COMMENT_START] !== undefined) {
                    // We started a weird comment, like </{
                    regex = comment2EndRegex;
                }
                else if (match[TAG_NAME] !== undefined) {
                    if (rawTextElement.test(match[TAG_NAME])) {
                        // Record if we encounter a raw-text element. We'll switch to
                        // this regex at the end of the tag.
                        rawTextEndRegex = new RegExp(`</${match[TAG_NAME]}`, 'g');
                    }
                    regex = tagEndRegex;
                }
                else if (match[DYNAMIC_TAG_NAME] !== undefined) {
                    if (DEV_MODE) {
                        throw new Error('Bindings in tag names are not supported. Please use static templates instead. ' +
                            'See https://lit.dev/docs/templates/expressions/#static-expressions');
                    }
                    regex = tagEndRegex;
                }
            }
            else if (regex === tagEndRegex) {
                if (match[ENTIRE_MATCH] === '>') {
                    // End of a tag. If we had started a raw-text element, use that
                    // regex
                    regex = rawTextEndRegex ?? textEndRegex;
                    // We may be ending an unquoted attribute value, so make sure we
                    // clear any pending attrNameEndIndex
                    attrNameEndIndex = -1;
                }
                else if (match[ATTRIBUTE_NAME] === undefined) {
                    // Attribute name position
                    attrNameEndIndex = -2;
                }
                else {
                    attrNameEndIndex = regex.lastIndex - match[SPACES_AND_EQUALS].length;
                    attrName = match[ATTRIBUTE_NAME];
                    regex =
                        match[QUOTE_CHAR] === undefined
                            ? tagEndRegex
                            : match[QUOTE_CHAR] === '"'
                                ? doubleQuoteAttrEndRegex
                                : singleQuoteAttrEndRegex;
                }
            }
            else if (regex === doubleQuoteAttrEndRegex ||
                regex === singleQuoteAttrEndRegex) {
                regex = tagEndRegex;
            }
            else if (regex === commentEndRegex || regex === comment2EndRegex) {
                regex = textEndRegex;
            }
            else {
                // Not one of the five state regexes, so it must be the dynamically
                // created raw text regex and we're at the close of that element.
                regex = tagEndRegex;
                rawTextEndRegex = undefined;
            }
        }
        if (DEV_MODE) {
            // If we have a attrNameEndIndex, which indicates that we should
            // rewrite the attribute name, assert that we're in a valid attribute
            // position - either in a tag, or a quoted attribute value.
            console.assert(attrNameEndIndex === -1 ||
                regex === tagEndRegex ||
                regex === singleQuoteAttrEndRegex ||
                regex === doubleQuoteAttrEndRegex, 'unexpected parse state B');
        }
        // We have four cases:
        //  1. We're in text position, and not in a raw text element
        //     (regex === textEndRegex): insert a comment marker.
        //  2. We have a non-negative attrNameEndIndex which means we need to
        //     rewrite the attribute name to add a bound attribute suffix.
        //  3. We're at the non-first binding in a multi-binding attribute, use a
        //     plain marker.
        //  4. We're somewhere else inside the tag. If we're in attribute name
        //     position (attrNameEndIndex === -2), add a sequential suffix to
        //     generate a unique attribute name.
        // Detect a binding next to self-closing tag end and insert a space to
        // separate the marker from the tag end:
        const end = regex === tagEndRegex && strings[i + 1].startsWith('/>') ? ' ' : '';
        html +=
            regex === textEndRegex
                ? s + nodeMarker
                : attrNameEndIndex >= 0
                    ? (attrNames.push(attrName),
                        s.slice(0, attrNameEndIndex) +
                            boundAttributeSuffix +
                            s.slice(attrNameEndIndex)) +
                        marker +
                        end
                    : s + marker + (attrNameEndIndex === -2 ? i : end);
    }
    const htmlResult = html +
        (strings[l] || '<?>') +
        (type === SVG_RESULT ? '</svg>' : type === MATHML_RESULT ? '</math>' : '');
    // Returned as an array for terseness
    return [trustFromTemplateString(strings, htmlResult), attrNames];
};
class Template {
    constructor(
    // This property needs to remain unminified.
    { strings, ['_$litType$']: type }, options) {
        this.parts = [];
        let node;
        let nodeIndex = 0;
        let attrNameIndex = 0;
        const partCount = strings.length - 1;
        const parts = this.parts;
        // Create template element
        const [html, attrNames] = getTemplateHtml(strings, type);
        this.el = Template.createElement(html, options);
        walker.currentNode = this.el.content;
        // Re-parent SVG or MathML nodes into template root
        if (type === SVG_RESULT || type === MATHML_RESULT) {
            const wrapper = this.el.content.firstChild;
            wrapper.replaceWith(...wrapper.childNodes);
        }
        // Walk the template to find binding markers and create TemplateParts
        while ((node = walker.nextNode()) !== null && parts.length < partCount) {
            if (node.nodeType === 1) {
                if (DEV_MODE) {
                    const tag = node.localName;
                    // Warn if `textarea` includes an expression and throw if `template`
                    // does since these are not supported. We do this by checking
                    // innerHTML for anything that looks like a marker. This catches
                    // cases like bindings in textarea there markers turn into text nodes.
                    if (/^(?:textarea|template)$/i.test(tag) &&
                        node.innerHTML.includes(marker)) {
                        const m = `Expressions are not supported inside \`${tag}\` ` +
                            `elements. See https://lit.dev/msg/expression-in-${tag} for more ` +
                            `information.`;
                        if (tag === 'template') {
                            throw new Error(m);
                        }
                        else
                            issueWarning('', m);
                    }
                }
                // TODO (justinfagnani): for attempted dynamic tag names, we don't
                // increment the bindingIndex, and it'll be off by 1 in the element
                // and off by two after it.
                if (node.hasAttributes()) {
                    for (const name of node.getAttributeNames()) {
                        if (name.endsWith(boundAttributeSuffix)) {
                            const realName = attrNames[attrNameIndex++];
                            const value = node.getAttribute(name);
                            const statics = value.split(marker);
                            const m = /([.?@])?(.*)/.exec(realName);
                            parts.push({
                                type: ATTRIBUTE_PART,
                                index: nodeIndex,
                                name: m[2],
                                strings: statics,
                                ctor: m[1] === '.'
                                    ? PropertyPart
                                    : m[1] === '?'
                                        ? BooleanAttributePart
                                        : m[1] === '@'
                                            ? EventPart
                                            : AttributePart,
                            });
                            node.removeAttribute(name);
                        }
                        else if (name.startsWith(marker)) {
                            parts.push({
                                type: ELEMENT_PART,
                                index: nodeIndex,
                            });
                            node.removeAttribute(name);
                        }
                    }
                }
                // TODO (justinfagnani): benchmark the regex against testing for each
                // of the 3 raw text element names.
                if (rawTextElement.test(node.tagName)) {
                    // For raw text elements we need to split the text content on
                    // markers, create a Text node for each segment, and create
                    // a TemplatePart for each marker.
                    const strings = node.textContent.split(marker);
                    const lastIndex = strings.length - 1;
                    if (lastIndex > 0) {
                        node.textContent = trustedTypes
                            ? trustedTypes.emptyScript
                            : '';
                        // Generate a new text node for each literal section
                        // These nodes are also used as the markers for child parts
                        for (let i = 0; i < lastIndex; i++) {
                            node.append(strings[i], createMarker());
                            // Walk past the marker node we just added
                            walker.nextNode();
                            parts.push({ type: CHILD_PART, index: ++nodeIndex });
                        }
                        // Note because this marker is added after the walker's current
                        // node, it will be walked to in the outer loop (and ignored), so
                        // we don't need to adjust nodeIndex here
                        node.append(strings[lastIndex], createMarker());
                    }
                }
            }
            else if (node.nodeType === 8) {
                const data = node.data;
                if (data === markerMatch) {
                    parts.push({ type: CHILD_PART, index: nodeIndex });
                }
                else {
                    let i = -1;
                    while ((i = node.data.indexOf(marker, i + 1)) !== -1) {
                        // Comment node has a binding marker inside, make an inactive part
                        // The binding won't work, but subsequent bindings will
                        parts.push({ type: COMMENT_PART, index: nodeIndex });
                        // Move to the end of the match
                        i += marker.length - 1;
                    }
                }
            }
            nodeIndex++;
        }
        if (DEV_MODE) {
            // If there was a duplicate attribute on a tag, then when the tag is
            // parsed into an element the attribute gets de-duplicated. We can detect
            // this mismatch if we haven't precisely consumed every attribute name
            // when preparing the template. This works because `attrNames` is built
            // from the template string and `attrNameIndex` comes from processing the
            // resulting DOM.
            if (attrNames.length !== attrNameIndex) {
                throw new Error(`Detected duplicate attribute bindings. This occurs if your template ` +
                    `has duplicate attributes on an element tag. For example ` +
                    `"<input ?disabled=\${true} ?disabled=\${false}>" contains a ` +
                    `duplicate "disabled" attribute. The error was detected in ` +
                    `the following template: \n` +
                    '`' +
                    strings.join('${...}') +
                    '`');
            }
        }
        // We could set walker.currentNode to another node here to prevent a memory
        // leak, but every time we prepare a template, we immediately render it
        // and re-use the walker in new TemplateInstance._clone().
        debugLogEvent &&
            debugLogEvent({
                kind: 'template prep',
                template: this,
                clonableTemplate: this.el,
                parts: this.parts,
                strings,
            });
    }
    // Overridden via `litHtmlPolyfillSupport` to provide platform support.
    /** @nocollapse */
    static createElement(html, _options) {
        const el = d.createElement('template');
        el.innerHTML = html;
        return el;
    }
}
function resolveDirective(part, value, parent = part, attributeIndex) {
    // Bail early if the value is explicitly noChange. Note, this means any
    // nested directive is still attached and is not run.
    if (value === noChange) {
        return value;
    }
    let currentDirective = attributeIndex !== undefined
        ? parent.__directives?.[attributeIndex]
        : parent.__directive;
    const nextDirectiveConstructor = isPrimitive(value)
        ? undefined
        : // This property needs to remain unminified.
            value['_$litDirective$'];
    if (currentDirective?.constructor !== nextDirectiveConstructor) {
        // This property needs to remain unminified.
        currentDirective?.['_$notifyDirectiveConnectionChanged']?.(false);
        if (nextDirectiveConstructor === undefined) {
            currentDirective = undefined;
        }
        else {
            currentDirective = new nextDirectiveConstructor(part);
            currentDirective._$initialize(part, parent, attributeIndex);
        }
        if (attributeIndex !== undefined) {
            (parent.__directives ??= [])[attributeIndex] =
                currentDirective;
        }
        else {
            parent.__directive = currentDirective;
        }
    }
    if (currentDirective !== undefined) {
        value = resolveDirective(part, currentDirective._$resolve(part, value.values), currentDirective, attributeIndex);
    }
    return value;
}
/**
 * An updateable instance of a Template. Holds references to the Parts used to
 * update the template instance.
 */
class TemplateInstance {
    constructor(template, parent) {
        this._$parts = [];
        /** @internal */
        this._$disconnectableChildren = undefined;
        this._$template = template;
        this._$parent = parent;
    }
    // Called by ChildPart parentNode getter
    get parentNode() {
        return this._$parent.parentNode;
    }
    // See comment in Disconnectable interface for why this is a getter
    get _$isConnected() {
        return this._$parent._$isConnected;
    }
    // This method is separate from the constructor because we need to return a
    // DocumentFragment and we don't want to hold onto it with an instance field.
    _clone(options) {
        const { el: { content }, parts: parts, } = this._$template;
        const fragment = (options?.creationScope ?? d).importNode(content, true);
        walker.currentNode = fragment;
        let node = walker.nextNode();
        let nodeIndex = 0;
        let partIndex = 0;
        let templatePart = parts[0];
        while (templatePart !== undefined) {
            if (nodeIndex === templatePart.index) {
                let part;
                if (templatePart.type === CHILD_PART) {
                    part = new ChildPart(node, node.nextSibling, this, options);
                }
                else if (templatePart.type === ATTRIBUTE_PART) {
                    part = new templatePart.ctor(node, templatePart.name, templatePart.strings, this, options);
                }
                else if (templatePart.type === ELEMENT_PART) {
                    part = new ElementPart(node, this, options);
                }
                this._$parts.push(part);
                templatePart = parts[++partIndex];
            }
            if (nodeIndex !== templatePart?.index) {
                node = walker.nextNode();
                nodeIndex++;
            }
        }
        // We need to set the currentNode away from the cloned tree so that we
        // don't hold onto the tree even if the tree is detached and should be
        // freed.
        walker.currentNode = d;
        return fragment;
    }
    _update(values) {
        let i = 0;
        for (const part of this._$parts) {
            if (part !== undefined) {
                debugLogEvent &&
                    debugLogEvent({
                        kind: 'set part',
                        part,
                        value: values[i],
                        valueIndex: i,
                        values,
                        templateInstance: this,
                    });
                if (part.strings !== undefined) {
                    part._$setValue(values, part, i);
                    // The number of values the part consumes is part.strings.length - 1
                    // since values are in between template spans. We increment i by 1
                    // later in the loop, so increment it by part.strings.length - 2 here
                    i += part.strings.length - 2;
                }
                else {
                    part._$setValue(values[i]);
                }
            }
            i++;
        }
    }
}
class ChildPart {
    // See comment in Disconnectable interface for why this is a getter
    get _$isConnected() {
        // ChildParts that are not at the root should always be created with a
        // parent; only RootChildNode's won't, so they return the local isConnected
        // state
        return this._$parent?._$isConnected ?? this.__isConnected;
    }
    constructor(startNode, endNode, parent, options) {
        this.type = CHILD_PART;
        this._$committedValue = nothing;
        // The following fields will be patched onto ChildParts when required by
        // AsyncDirective
        /** @internal */
        this._$disconnectableChildren = undefined;
        this._$startNode = startNode;
        this._$endNode = endNode;
        this._$parent = parent;
        this.options = options;
        // Note __isConnected is only ever accessed on RootParts (i.e. when there is
        // no _$parent); the value on a non-root-part is "don't care", but checking
        // for parent would be more code
        this.__isConnected = options?.isConnected ?? true;
        if (ENABLE_EXTRA_SECURITY_HOOKS) {
            // Explicitly initialize for consistent class shape.
            this._textSanitizer = undefined;
        }
    }
    /**
     * The parent node into which the part renders its content.
     *
     * A ChildPart's content consists of a range of adjacent child nodes of
     * `.parentNode`, possibly bordered by 'marker nodes' (`.startNode` and
     * `.endNode`).
     *
     * - If both `.startNode` and `.endNode` are non-null, then the part's content
     * consists of all siblings between `.startNode` and `.endNode`, exclusively.
     *
     * - If `.startNode` is non-null but `.endNode` is null, then the part's
     * content consists of all siblings following `.startNode`, up to and
     * including the last child of `.parentNode`. If `.endNode` is non-null, then
     * `.startNode` will always be non-null.
     *
     * - If both `.endNode` and `.startNode` are null, then the part's content
     * consists of all child nodes of `.parentNode`.
     */
    get parentNode() {
        let parentNode = wrap(this._$startNode).parentNode;
        const parent = this._$parent;
        if (parent !== undefined &&
            parentNode?.nodeType === 11 /* Node.DOCUMENT_FRAGMENT */) {
            // If the parentNode is a DocumentFragment, it may be because the DOM is
            // still in the cloned fragment during initial render; if so, get the real
            // parentNode the part will be committed into by asking the parent.
            parentNode = parent.parentNode;
        }
        return parentNode;
    }
    /**
     * The part's leading marker node, if any. See `.parentNode` for more
     * information.
     */
    get startNode() {
        return this._$startNode;
    }
    /**
     * The part's trailing marker node, if any. See `.parentNode` for more
     * information.
     */
    get endNode() {
        return this._$endNode;
    }
    _$setValue(value, directiveParent = this) {
        if (DEV_MODE && this.parentNode === null) {
            throw new Error(`This \`ChildPart\` has no \`parentNode\` and therefore cannot accept a value. This likely means the element containing the part was manipulated in an unsupported way outside of Lit's control such that the part's marker nodes were ejected from DOM. For example, setting the element's \`innerHTML\` or \`textContent\` can do this.`);
        }
        value = resolveDirective(this, value, directiveParent);
        if (isPrimitive(value)) {
            // Non-rendering child values. It's important that these do not render
            // empty text nodes to avoid issues with preventing default <slot>
            // fallback content.
            if (value === nothing || value == null || value === '') {
                if (this._$committedValue !== nothing) {
                    debugLogEvent &&
                        debugLogEvent({
                            kind: 'commit nothing to child',
                            start: this._$startNode,
                            end: this._$endNode,
                            parent: this._$parent,
                            options: this.options,
                        });
                    this._$clear();
                }
                this._$committedValue = nothing;
            }
            else if (value !== this._$committedValue && value !== noChange) {
                this._commitText(value);
            }
            // This property needs to remain unminified.
        }
        else if (value['_$litType$'] !== undefined) {
            this._commitTemplateResult(value);
        }
        else if (value.nodeType !== undefined) {
            if (DEV_MODE && this.options?.host === value) {
                this._commitText(`[probable mistake: rendered a template's host in itself ` +
                    `(commonly caused by writing \${this} in a template]`);
                console.warn(`Attempted to render the template host`, value, `inside itself. This is almost always a mistake, and in dev mode `, `we render some warning text. In production however, we'll `, `render it, which will usually result in an error, and sometimes `, `in the element disappearing from the DOM.`);
                return;
            }
            this._commitNode(value);
        }
        else if (isIterable(value)) {
            this._commitIterable(value);
        }
        else {
            // Fallback, will render the string representation
            this._commitText(value);
        }
    }
    _insert(node) {
        return wrap(wrap(this._$startNode).parentNode).insertBefore(node, this._$endNode);
    }
    _commitNode(value) {
        if (this._$committedValue !== value) {
            this._$clear();
            if (ENABLE_EXTRA_SECURITY_HOOKS &&
                sanitizerFactoryInternal !== noopSanitizer) {
                const parentNodeName = this._$startNode.parentNode?.nodeName;
                if (parentNodeName === 'STYLE' || parentNodeName === 'SCRIPT') {
                    let message = 'Forbidden';
                    if (DEV_MODE) {
                        if (parentNodeName === 'STYLE') {
                            message =
                                `Lit does not support binding inside style nodes. ` +
                                    `This is a security risk, as style injection attacks can ` +
                                    `exfiltrate data and spoof UIs. ` +
                                    `Consider instead using css\`...\` literals ` +
                                    `to compose styles, and do dynamic styling with ` +
                                    `css custom properties, ::parts, <slot>s, ` +
                                    `and by mutating the DOM rather than stylesheets.`;
                        }
                        else {
                            message =
                                `Lit does not support binding inside script nodes. ` +
                                    `This is a security risk, as it could allow arbitrary ` +
                                    `code execution.`;
                        }
                    }
                    throw new Error(message);
                }
            }
            debugLogEvent &&
                debugLogEvent({
                    kind: 'commit node',
                    start: this._$startNode,
                    parent: this._$parent,
                    value: value,
                    options: this.options,
                });
            this._$committedValue = this._insert(value);
        }
    }
    _commitText(value) {
        // If the committed value is a primitive it means we called _commitText on
        // the previous render, and we know that this._$startNode.nextSibling is a
        // Text node. We can now just replace the text content (.data) of the node.
        if (this._$committedValue !== nothing &&
            isPrimitive(this._$committedValue)) {
            const node = wrap(this._$startNode).nextSibling;
            if (ENABLE_EXTRA_SECURITY_HOOKS) {
                if (this._textSanitizer === undefined) {
                    this._textSanitizer = createSanitizer(node, 'data', 'property');
                }
                value = this._textSanitizer(value);
            }
            debugLogEvent &&
                debugLogEvent({
                    kind: 'commit text',
                    node,
                    value,
                    options: this.options,
                });
            node.data = value;
        }
        else {
            if (ENABLE_EXTRA_SECURITY_HOOKS) {
                const textNode = d.createTextNode('');
                this._commitNode(textNode);
                // When setting text content, for security purposes it matters a lot
                // what the parent is. For example, <style> and <script> need to be
                // handled with care, while <span> does not. So first we need to put a
                // text node into the document, then we can sanitize its content.
                if (this._textSanitizer === undefined) {
                    this._textSanitizer = createSanitizer(textNode, 'data', 'property');
                }
                value = this._textSanitizer(value);
                debugLogEvent &&
                    debugLogEvent({
                        kind: 'commit text',
                        node: textNode,
                        value,
                        options: this.options,
                    });
                textNode.data = value;
            }
            else {
                this._commitNode(d.createTextNode(value));
                debugLogEvent &&
                    debugLogEvent({
                        kind: 'commit text',
                        node: wrap(this._$startNode).nextSibling,
                        value,
                        options: this.options,
                    });
            }
        }
        this._$committedValue = value;
    }
    _commitTemplateResult(result) {
        // This property needs to remain unminified.
        const { values, ['_$litType$']: type } = result;
        // If $litType$ is a number, result is a plain TemplateResult and we get
        // the template from the template cache. If not, result is a
        // CompiledTemplateResult and _$litType$ is a CompiledTemplate and we need
        // to create the <template> element the first time we see it.
        const template = typeof type === 'number'
            ? this._$getTemplate(result)
            : (type.el === undefined &&
                (type.el = Template.createElement(trustFromTemplateString(type.h, type.h[0]), this.options)),
                type);
        if (this._$committedValue?._$template === template) {
            debugLogEvent &&
                debugLogEvent({
                    kind: 'template updating',
                    template,
                    instance: this._$committedValue,
                    parts: this._$committedValue._$parts,
                    options: this.options,
                    values,
                });
            this._$committedValue._update(values);
        }
        else {
            const instance = new TemplateInstance(template, this);
            const fragment = instance._clone(this.options);
            debugLogEvent &&
                debugLogEvent({
                    kind: 'template instantiated',
                    template,
                    instance,
                    parts: instance._$parts,
                    options: this.options,
                    fragment,
                    values,
                });
            instance._update(values);
            debugLogEvent &&
                debugLogEvent({
                    kind: 'template instantiated and updated',
                    template,
                    instance,
                    parts: instance._$parts,
                    options: this.options,
                    fragment,
                    values,
                });
            this._commitNode(fragment);
            this._$committedValue = instance;
        }
    }
    // Overridden via `litHtmlPolyfillSupport` to provide platform support.
    /** @internal */
    _$getTemplate(result) {
        let template = templateCache.get(result.strings);
        if (template === undefined) {
            templateCache.set(result.strings, (template = new Template(result)));
        }
        return template;
    }
    _commitIterable(value) {
        // For an Iterable, we create a new InstancePart per item, then set its
        // value to the item. This is a little bit of overhead for every item in
        // an Iterable, but it lets us recurse easily and efficiently update Arrays
        // of TemplateResults that will be commonly returned from expressions like:
        // array.map((i) => html`${i}`), by reusing existing TemplateInstances.
        // If value is an array, then the previous render was of an
        // iterable and value will contain the ChildParts from the previous
        // render. If value is not an array, clear this part and make a new
        // array for ChildParts.
        if (!isArray(this._$committedValue)) {
            this._$committedValue = [];
            this._$clear();
        }
        // Lets us keep track of how many items we stamped so we can clear leftover
        // items from a previous render
        const itemParts = this._$committedValue;
        let partIndex = 0;
        let itemPart;
        for (const item of value) {
            if (partIndex === itemParts.length) {
                // If no existing part, create a new one
                // TODO (justinfagnani): test perf impact of always creating two parts
                // instead of sharing parts between nodes
                // https://github.com/lit/lit/issues/1266
                itemParts.push((itemPart = new ChildPart(this._insert(createMarker()), this._insert(createMarker()), this, this.options)));
            }
            else {
                // Reuse an existing part
                itemPart = itemParts[partIndex];
            }
            itemPart._$setValue(item);
            partIndex++;
        }
        if (partIndex < itemParts.length) {
            // itemParts always have end nodes
            this._$clear(itemPart && wrap(itemPart._$endNode).nextSibling, partIndex);
            // Truncate the parts array so _value reflects the current state
            itemParts.length = partIndex;
        }
    }
    /**
     * Removes the nodes contained within this Part from the DOM.
     *
     * @param start Start node to clear from, for clearing a subset of the part's
     *     DOM (used when truncating iterables)
     * @param from  When `start` is specified, the index within the iterable from
     *     which ChildParts are being removed, used for disconnecting directives
     *     in those Parts.
     *
     * @internal
     */
    _$clear(start = wrap(this._$startNode).nextSibling, from) {
        this._$notifyConnectionChanged?.(false, true, from);
        while (start !== this._$endNode) {
            // The non-null assertion is safe because if _$startNode.nextSibling is
            // null, then _$endNode is also null, and we would not have entered this
            // loop.
            const n = wrap(start).nextSibling;
            wrap(start).remove();
            start = n;
        }
    }
    /**
     * Implementation of RootPart's `isConnected`. Note that this method
     * should only be called on `RootPart`s (the `ChildPart` returned from a
     * top-level `render()` call). It has no effect on non-root ChildParts.
     * @param isConnected Whether to set
     * @internal
     */
    setConnected(isConnected) {
        if (this._$parent === undefined) {
            this.__isConnected = isConnected;
            this._$notifyConnectionChanged?.(isConnected);
        }
        else if (DEV_MODE) {
            throw new Error('part.setConnected() may only be called on a ' +
                'RootPart returned from render().');
        }
    }
}
class AttributePart {
    get tagName() {
        return this.element.tagName;
    }
    // See comment in Disconnectable interface for why this is a getter
    get _$isConnected() {
        return this._$parent._$isConnected;
    }
    constructor(element, name, strings, parent, options) {
        this.type = ATTRIBUTE_PART;
        /** @internal */
        this._$committedValue = nothing;
        /** @internal */
        this._$disconnectableChildren = undefined;
        this.element = element;
        this.name = name;
        this._$parent = parent;
        this.options = options;
        if (strings.length > 2 || strings[0] !== '' || strings[1] !== '') {
            this._$committedValue = new Array(strings.length - 1).fill(new String());
            this.strings = strings;
        }
        else {
            this._$committedValue = nothing;
        }
        if (ENABLE_EXTRA_SECURITY_HOOKS) {
            this._sanitizer = undefined;
        }
    }
    /**
     * Sets the value of this part by resolving the value from possibly multiple
     * values and static strings and committing it to the DOM.
     * If this part is single-valued, `this._strings` will be undefined, and the
     * method will be called with a single value argument. If this part is
     * multi-value, `this._strings` will be defined, and the method is called
     * with the value array of the part's owning TemplateInstance, and an offset
     * into the value array from which the values should be read.
     * This method is overloaded this way to eliminate short-lived array slices
     * of the template instance values, and allow a fast-path for single-valued
     * parts.
     *
     * @param value The part value, or an array of values for multi-valued parts
     * @param valueIndex the index to start reading values from. `undefined` for
     *   single-valued parts
     * @param noCommit causes the part to not commit its value to the DOM. Used
     *   in hydration to prime attribute parts with their first-rendered value,
     *   but not set the attribute, and in SSR to no-op the DOM operation and
     *   capture the value for serialization.
     *
     * @internal
     */
    _$setValue(value, directiveParent = this, valueIndex, noCommit) {
        const strings = this.strings;
        // Whether any of the values has changed, for dirty-checking
        let change = false;
        if (strings === undefined) {
            // Single-value binding case
            value = resolveDirective(this, value, directiveParent, 0);
            change =
                !isPrimitive(value) ||
                    (value !== this._$committedValue && value !== noChange);
            if (change) {
                this._$committedValue = value;
            }
        }
        else {
            // Interpolation case
            const values = value;
            value = strings[0];
            let i, v;
            for (i = 0; i < strings.length - 1; i++) {
                v = resolveDirective(this, values[valueIndex + i], directiveParent, i);
                if (v === noChange) {
                    // If the user-provided value is `noChange`, use the previous value
                    v = this._$committedValue[i];
                }
                change ||=
                    !isPrimitive(v) || v !== this._$committedValue[i];
                if (v === nothing) {
                    value = nothing;
                }
                else if (value !== nothing) {
                    value += (v ?? '') + strings[i + 1];
                }
                // We always record each value, even if one is `nothing`, for future
                // change detection.
                this._$committedValue[i] = v;
            }
        }
        if (change && !noCommit) {
            this._commitValue(value);
        }
    }
    /** @internal */
    _commitValue(value) {
        if (value === nothing) {
            wrap(this.element).removeAttribute(this.name);
        }
        else {
            if (ENABLE_EXTRA_SECURITY_HOOKS) {
                if (this._sanitizer === undefined) {
                    this._sanitizer = sanitizerFactoryInternal(this.element, this.name, 'attribute');
                }
                value = this._sanitizer(value ?? '');
            }
            debugLogEvent &&
                debugLogEvent({
                    kind: 'commit attribute',
                    element: this.element,
                    name: this.name,
                    value,
                    options: this.options,
                });
            wrap(this.element).setAttribute(this.name, (value ?? ''));
        }
    }
}
class PropertyPart extends AttributePart {
    constructor() {
        super(...arguments);
        this.type = PROPERTY_PART;
    }
    /** @internal */
    _commitValue(value) {
        if (ENABLE_EXTRA_SECURITY_HOOKS) {
            if (this._sanitizer === undefined) {
                this._sanitizer = sanitizerFactoryInternal(this.element, this.name, 'property');
            }
            value = this._sanitizer(value);
        }
        debugLogEvent &&
            debugLogEvent({
                kind: 'commit property',
                element: this.element,
                name: this.name,
                value,
                options: this.options,
            });
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        this.element[this.name] = value === nothing ? undefined : value;
    }
}
class BooleanAttributePart extends AttributePart {
    constructor() {
        super(...arguments);
        this.type = BOOLEAN_ATTRIBUTE_PART;
    }
    /** @internal */
    _commitValue(value) {
        debugLogEvent &&
            debugLogEvent({
                kind: 'commit boolean attribute',
                element: this.element,
                name: this.name,
                value: !!(value && value !== nothing),
                options: this.options,
            });
        wrap(this.element).toggleAttribute(this.name, !!value && value !== nothing);
    }
}
class EventPart extends AttributePart {
    constructor(element, name, strings, parent, options) {
        super(element, name, strings, parent, options);
        this.type = EVENT_PART;
        if (DEV_MODE && this.strings !== undefined) {
            throw new Error(`A \`<${element.localName}>\` has a \`@${name}=...\` listener with ` +
                'invalid content. Event listeners in templates must have exactly ' +
                'one expression and no surrounding text.');
        }
    }
    // EventPart does not use the base _$setValue/_resolveValue implementation
    // since the dirty checking is more complex
    /** @internal */
    _$setValue(newListener, directiveParent = this) {
        newListener =
            resolveDirective(this, newListener, directiveParent, 0) ?? nothing;
        if (newListener === noChange) {
            return;
        }
        const oldListener = this._$committedValue;
        // If the new value is nothing or any options change we have to remove the
        // part as a listener.
        const shouldRemoveListener = (newListener === nothing && oldListener !== nothing) ||
            newListener.capture !==
                oldListener.capture ||
            newListener.once !==
                oldListener.once ||
            newListener.passive !==
                oldListener.passive;
        // If the new value is not nothing and we removed the listener, we have
        // to add the part as a listener.
        const shouldAddListener = newListener !== nothing &&
            (oldListener === nothing || shouldRemoveListener);
        debugLogEvent &&
            debugLogEvent({
                kind: 'commit event listener',
                element: this.element,
                name: this.name,
                value: newListener,
                options: this.options,
                removeListener: shouldRemoveListener,
                addListener: shouldAddListener,
                oldListener,
            });
        if (shouldRemoveListener) {
            this.element.removeEventListener(this.name, this, oldListener);
        }
        if (shouldAddListener) {
            this.element.addEventListener(this.name, this, newListener);
        }
        this._$committedValue = newListener;
    }
    handleEvent(event) {
        if (typeof this._$committedValue === 'function') {
            this._$committedValue.call(this.options?.host ?? this.element, event);
        }
        else {
            this._$committedValue.handleEvent(event);
        }
    }
}
class ElementPart {
    constructor(element, parent, options) {
        this.element = element;
        this.type = ELEMENT_PART;
        /** @internal */
        this._$disconnectableChildren = undefined;
        this._$parent = parent;
        this.options = options;
    }
    // See comment in Disconnectable interface for why this is a getter
    get _$isConnected() {
        return this._$parent._$isConnected;
    }
    _$setValue(value) {
        debugLogEvent &&
            debugLogEvent({
                kind: 'commit to element binding',
                element: this.element,
                value,
                options: this.options,
            });
        resolveDirective(this, value);
    }
}
/**
 * END USERS SHOULD NOT RELY ON THIS OBJECT.
 *
 * Private exports for use by other Lit packages, not intended for use by
 * external users.
 *
 * We currently do not make a mangled rollup build of the lit-ssr code. In order
 * to keep a number of (otherwise private) top-level exports mangled in the
 * client side code, we export a _$LH object containing those members (or
 * helper methods for accessing private fields of those members), and then
 * re-export them for use in lit-ssr. This keeps lit-ssr agnostic to whether the
 * client-side code is being used in `dev` mode or `prod` mode.
 *
 * This has a unique name, to disambiguate it from private exports in
 * lit-element, which re-exports all of lit-html.
 *
 * @private
 */
const _$LH = {
    // Used in lit-ssr
    _boundAttributeSuffix: boundAttributeSuffix,
    _marker: marker,
    _markerMatch: markerMatch,
    _HTML_RESULT: HTML_RESULT,
    _getTemplateHtml: getTemplateHtml,
    // Used in tests and private-ssr-support
    _TemplateInstance: TemplateInstance,
    _isIterable: isIterable,
    _resolveDirective: resolveDirective,
    _ChildPart: ChildPart,
    _AttributePart: AttributePart,
    _BooleanAttributePart: BooleanAttributePart,
    _EventPart: EventPart,
    _PropertyPart: PropertyPart,
    _ElementPart: ElementPart,
};
// Apply polyfills if available
const polyfillSupport = DEV_MODE
    ? global.litHtmlPolyfillSupportDevMode
    : global.litHtmlPolyfillSupport;
polyfillSupport?.(Template, ChildPart);
// IMPORTANT: do not change the property name or the assignment expression.
// This line will be used in regexes to search for lit-html usage.
(global.litHtmlVersions ??= []).push('3.3.1');
if (DEV_MODE && global.litHtmlVersions.length > 1) {
    queueMicrotask(() => {
        issueWarning('multiple-versions', `Multiple versions of Lit loaded. ` +
            `Loading multiple versions is not recommended.`);
    });
}
/**
 * Renders a value, usually a lit-html TemplateResult, to the container.
 *
 * This example renders the text "Hello, Zoe!" inside a paragraph tag, appending
 * it to the container `document.body`.
 *
 * ```js
 * import {html, render} from 'lit';
 *
 * const name = "Zoe";
 * render(html`<p>Hello, ${name}!</p>`, document.body);
 * ```
 *
 * @param value Any [renderable
 *   value](https://lit.dev/docs/templates/expressions/#child-expressions),
 *   typically a {@linkcode TemplateResult} created by evaluating a template tag
 *   like {@linkcode html} or {@linkcode svg}.
 * @param container A DOM container to render to. The first render will append
 *   the rendered value to the container, and subsequent renders will
 *   efficiently update the rendered value if the same result type was
 *   previously rendered there.
 * @param options See {@linkcode RenderOptions} for options documentation.
 * @see
 * {@link https://lit.dev/docs/libraries/standalone-templates/#rendering-lit-html-templates| Rendering Lit HTML Templates}
 */
const render = (value, container, options) => {
    if (DEV_MODE && container == null) {
        // Give a clearer error message than
        //     Uncaught TypeError: Cannot read properties of null (reading
        //     '_$litPart$')
        // which reads like an internal Lit error.
        throw new TypeError(`The container to render into may not be ${container}`);
    }
    const renderId = DEV_MODE ? debugLogRenderId++ : 0;
    const partOwnerNode = options?.renderBefore ?? container;
    // This property needs to remain unminified.
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    let part = partOwnerNode['_$litPart$'];
    debugLogEvent &&
        debugLogEvent({
            kind: 'begin render',
            id: renderId,
            value,
            container,
            options,
            part,
        });
    if (part === undefined) {
        const endNode = options?.renderBefore ?? null;
        // This property needs to remain unminified.
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        partOwnerNode['_$litPart$'] = part = new ChildPart(container.insertBefore(createMarker(), endNode), endNode, undefined, options ?? {});
    }
    part._$setValue(value);
    debugLogEvent &&
        debugLogEvent({
            kind: 'end render',
            id: renderId,
            value,
            container,
            options,
            part,
        });
    return part;
};
if (ENABLE_EXTRA_SECURITY_HOOKS) {
    render.setSanitizer = setSanitizer;
    render.createSanitizer = createSanitizer;
    if (DEV_MODE) {
        render._testOnlyClearSanitizerFactoryDoNotCallOrElse =
            _testOnlyClearSanitizerFactoryDoNotCallOrElse;
    }
}
//# sourceMappingURL=lit-html.js.map

}),
"./node_modules/.pnpm/lit@3.3.1/node_modules/lit/index.js": 
/*!****************************************************************!*\
  !*** ./node_modules/.pnpm/lit@3.3.1/node_modules/lit/index.js ***!
  \****************************************************************/
(function (__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) {
__webpack_require__.r(__webpack_exports__);
__webpack_require__.d(__webpack_exports__, {
  CSSResult: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.CSSResult),
  LitElement: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.LitElement),
  ReactiveElement: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.ReactiveElement),
  _$LE: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__._$LE),
  _$LH: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__._$LH),
  adoptStyles: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.adoptStyles),
  css: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.css),
  defaultConverter: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.defaultConverter),
  getCompatibleStyle: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.getCompatibleStyle),
  html: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.html),
  isServer: () => (/* reexport safe */ lit_html_is_server_js__WEBPACK_IMPORTED_MODULE_3__.isServer),
  mathml: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.mathml),
  noChange: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.noChange),
  notEqual: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.notEqual),
  nothing: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.nothing),
  render: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.render),
  supportsAdoptingStyleSheets: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.supportsAdoptingStyleSheets),
  svg: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.svg),
  unsafeCSS: () => (/* reexport safe */ lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__.unsafeCSS)
});
/* ESM import */var _lit_reactive_element__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @lit/reactive-element */ "./node_modules/.pnpm/@lit+reactive-element@2.1.1/node_modules/@lit/reactive-element/development/reactive-element.js");
/* ESM import */var lit_html__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! lit-html */ "./node_modules/.pnpm/lit-html@3.3.1/node_modules/lit-html/development/lit-html.js");
/* ESM import */var lit_element_lit_element_js__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! lit-element/lit-element.js */ "./node_modules/.pnpm/lit-element@4.2.1/node_modules/lit-element/development/lit-element.js");
/* ESM import */var lit_html_is_server_js__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! lit-html/is-server.js */ "./node_modules/.pnpm/lit-html@3.3.1/node_modules/lit-html/development/is-server.js");

//# sourceMappingURL=index.js.map


}),

});
/************************************************************************/
// The module cache
var __webpack_module_cache__ = {};

// The require function
function __webpack_require__(moduleId) {

// Check if module is in cache
var cachedModule = __webpack_module_cache__[moduleId];
if (cachedModule !== undefined) {
return cachedModule.exports;
}
// Create a new module (and put it into the cache)
var module = (__webpack_module_cache__[moduleId] = {
exports: {}
});
// Execute the module function
__webpack_modules__[moduleId](module, module.exports, __webpack_require__);

// Return the exports of the module
return module.exports;

}

/************************************************************************/
// webpack/runtime/define_property_getters
(() => {
__webpack_require__.d = (exports, definition) => {
	for(var key in definition) {
        if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
            Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
        }
    }
};
})();
// webpack/runtime/has_own_property
(() => {
__webpack_require__.o = (obj, prop) => (Object.prototype.hasOwnProperty.call(obj, prop))
})();
// webpack/runtime/make_namespace_object
(() => {
// define __esModule on exports
__webpack_require__.r = (exports) => {
	if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
		Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
	}
	Object.defineProperty(exports, '__esModule', { value: true });
};
})();
// webpack/runtime/rspack_version
(() => {
__webpack_require__.rv = () => ("1.5.8")
})();
// webpack/runtime/rspack_unique_id
(() => {
__webpack_require__.ruid = "bundler=rspack@1.5.8";

})();
/************************************************************************/
var __webpack_exports__ = {};
// This entry needs to be wrapped in an IIFE because it needs to be isolated against other modules in the chunk.
(() => {

/*!*********************!*\
  !*** ./src/main.ts ***!
  \*********************/
__webpack_require__.r(__webpack_exports__);
/* ESM import */var uplot__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! uplot */ "./node_modules/.pnpm/uplot@1.6.32/node_modules/uplot/dist/uPlot.esm.js");
/* ESM import */var js_yaml__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! js-yaml */ "./node_modules/.pnpm/js-yaml@4.1.0/node_modules/js-yaml/dist/js-yaml.mjs");
/* ESM import */var _lib_components_meteo_dashboard__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./lib/components/meteo-dashboard */ "./src/lib/components/meteo-dashboard.ts");




})();

})()
;
//# sourceMappingURL=main.js.map