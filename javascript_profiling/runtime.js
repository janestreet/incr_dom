//Provides: js_performance
var js_performance = {mark: (function () {}), measure: (function () {}) };
if (typeof joo_global_object.performance !== 'undefined') {
    js_performance = joo_global_object.performance;
}

//Provides: js_prof_mark
//Requires: caml_jsbytes_of_string,js_performance
function js_prof_mark(name) {
    try {
        js_performance.mark(caml_jsbytes_of_string(name));
    } catch (e) {
        joo_global_object.console.warn(e);
    }
    return 0;
}

//Provides: js_prof_measure
//Requires: caml_jsbytes_of_string,js_performance
function js_prof_measure(name, start, end) {
    try {
        js_performance.measure(
            caml_jsbytes_of_string(name), 
            caml_jsbytes_of_string(start), 
            caml_jsbytes_of_string(end));
    } catch (e) {
        joo_global_object.console.warn(e);
    }
    return 0;
}
