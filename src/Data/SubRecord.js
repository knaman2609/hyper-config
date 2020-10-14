// originated from https://raw.githubusercontent.com/rubenpieters/purescript-subrecord/13fae32237f79ac2236c2e1e00147c0d51fd8b72/src/Data/SubRecord/Builder.js
"use strict";

exports.copyRecord = function (rec) {
  var copy = {};
  for (var key in rec) {
    if ({}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  return copy;
};

exports.unsafeInsert = function (l) {
  return function (a) {
    return function (rec) {
      rec[l] = a;
      return rec;
    };
  };
};

exports.unsafeModify = function (l) {
  return function (f) {
    return function (rec) {
      // only mutate if `rec` has property `l`
      if ({}.hasOwnProperty.call(rec, l)) {
        rec[l] = f(rec[l]);
      }
      return rec;
    };
  };
};

exports.unsafeDelete = function (l) {
  return function (rec) {
    delete rec[l];
    return rec;
  };
};

exports.unsafeRename = function (l1) {
  return function (l2) {
    return function (rec) {
      // only mutate if `rec` has property `l1`
      if ({}.hasOwnProperty.call(rec, l1)) {
        rec[l2] = rec[l1];
        delete rec[l1];
      }
      return rec;
    };
  };
};

exports.unsafeMerge = function (r1) {
  return function (r2) {
    var copy = {};
    for (var k1 in r2) {
      if ({}.hasOwnProperty.call(r2, k1)) {
        copy[k1] = r2[k1];
      }
    }
    for (var k2 in r1) {
      if ({}.hasOwnProperty.call(r1, k2)) {
        copy[k2] = r1[k2];
      }
    }
    return copy;
  };
};

exports["unsafeGetImpl"] = function unsafeGetImpl(label, rec) {
  return rec[label];
};
