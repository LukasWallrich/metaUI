// Browser-free checks of the actual exported scripts with controlled timers.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const templates = process.argv[2];
function page() {
  const elements = [], timers = new Map(); let receive, sequence = 0;
  const live = {contentWindow: {scrollTo() {}}, focus() {}};
  const root = {inert: true, querySelector: () => live};
  const document = {
    documentElement: {dataset: {metauiStartupTimeout: '25'}},
    createElement: tag => ({tag, style: {}, contentWindow: {scrollY: 0},
      setAttribute(k,v) { this[k] = v; }, remove() { this.removed = true; }}),
    head: {appendChild(e) {elements.push(e);}}, body: {appendChild(e) {elements.push(e);}},
    getElementById: () => root, addEventListener() {}
  };
  const context = {document, location: {search: '', origin: 'https://example.org'},
    URLSearchParams, window: {addEventListener(type, fn) {receive = fn;}},
    setTimeout(fn, ms) { const id = ++sequence; timers.set(id, {fn, ms}); return id; },
    clearTimeout(id) {timers.delete(id);}};
  vm.runInNewContext(fs.readFileSync(path.join(templates, 'metaui-preview.js'), 'utf8'), context);
  const status = () => elements.find(e => e.id === 'metaui-startup-status');
  assert.equal(status(), undefined); // Normal loading has no banner.
  assert.equal([...timers.values()][0].ms, 25);
  [...timers.values()][0].fn();
  assert.equal(status().role, 'status');
  assert.match(status().textContent, /modern desktop browser/);
  assert.equal(root.inert, true);
  assert.equal(elements.find(e => e.id === 'metaui-preview').removed, undefined);
  receive({source: {}, origin: context.location.origin, data: 'metaui-live-ready'});
  assert.equal(root.inert, true); // Ignore messages from other frames.
  receive({source: live.contentWindow, origin: context.location.origin, data: 'metaui-live-ready'});
  assert.equal(root.inert, false);
  assert.equal(status().removed, true);
  assert.equal(context.document.documentElement.dataset.metauiLive, 'true');
  assert.equal(timers.size, 0);
}
function bridge(mode) {
  let interval, deadline, clicked = 0, intervalCleared = false, deadlineCleared = false;
  const messages = [];
  const connected = mode !== 'disconnected';
  const parent = {metauiPrecomputed: true, location: {search: ''},
    document: {documentElement: {dataset: {metauiStartupTimeout: '25'}}},
    postMessage(message) {messages.push(message);}};
  const context = {window: {parent, Shiny: {}},
    Shiny: {shinyapp: {isConnected: () => connected, $inputValues: {outliers_z_scores: [-10,10]}}},
    document: {documentElement: {dataset: {}, classList: {contains: () => mode === 'busy'}},
      getElementById(id) {return id === 'go' ? {click() {clicked++;}} : {textContent: mode === 'success' ? 'result' : ''};},
      querySelector() {return mode === 'error' ? {} : null;}, addEventListener() {}},
    location: {pathname: '/app_test/', search: '', origin: 'https://example.org'},
    history: {replaceState() {}, pushState() {}}, URLSearchParams,
    setTimeout(fn, ms) {deadline = fn; assert.equal(ms, 25); return 1;},
    clearTimeout() {deadlineCleared = true;},
    setInterval(fn) {interval = fn; return 2;}, clearInterval() {intervalCleared = true;}};
  vm.runInNewContext(fs.readFileSync(path.join(templates, 'metaui-static.js'), 'utf8'), context);
  interval();
  if (['busy', 'disconnected'].includes(mode)) {assert.deepEqual(messages, []); deadline();}
  assert.deepEqual(messages, [mode === 'disconnected' ? 'metaui-startup-failed' : 'metaui-live-ready']);
  assert.equal(clicked, connected ? 1 : 0);
  assert.equal(intervalCleared, true);
  if (connected) assert.equal(deadlineCleared, true);
}
page();
for (const mode of ['success', 'error', 'busy', 'disconnected']) bridge(mode);
console.log('Startup timeout, recovery, analysis failure and successful handover passed.');
