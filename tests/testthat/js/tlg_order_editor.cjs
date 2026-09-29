// Execute the actual R-generated cell functions without third-party Node packages.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const definitions = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));

for (const definition of definitions) {
  const events = [];
  const context = vm.createContext({
    React: { createElement: (type, props) => ({ type, props }) },
    Shiny: { setInputValue: (...args) => events.push(args) }
  });
  const render = vm.runInContext(definition.initial, context);
  const row = { index: 3, column: { id: definition.field }, value: 'Original text' };
  const first = render(row);
  assert.equal(first.props.defaultValue, 'Original text');
  first.props.onInput({ target: { value: 'Unsaved edit' } });
  assert.deepEqual(JSON.parse(JSON.stringify(events.at(-1))), [
    definition.id,
    { row: 4, column: definition.field, value: 'Unsaved edit' },
    { priority: 'event' }
  ]);

  // Sorting or paging can unmount cells while the server snapshot stays the same.
  assert.equal(render({ ...row, index: 0, value: 'Another row' }).props.defaultValue, 'Another row');
  assert.equal(render(row).props.defaultValue, 'Unsaved edit');
  render(row).props.onInput({ target: { value: '' } });
  assert.equal(render(row).props.defaultValue, '');

  // Even restoring the original text must replace an already-edited input.
  const restoreOriginal = vm.runInContext(definition.restored, context);
  const original = restoreOriginal(row);
  assert.equal(original.props.defaultValue, 'Original text');
  assert.notEqual(original.props.key, first.props.key);

  const restoreChanged = vm.runInContext(definition.restored, context);
  const changed = restoreChanged({ ...row, value: 'Restored text' });
  assert.equal(changed.props.defaultValue, 'Restored text');
  assert.equal(restoreChanged({ ...row, index: 0, value: null }).props.defaultValue, '');
  changed.props.onInput({ target: { value: 'Edit after restore' } });
  assert.equal(restoreChanged(row).props.defaultValue, 'Edit after restore');
  assert.equal(events.at(-1)[1].row, 4);
  assert.equal(events.at(-1)[1].value, 'Edit after restore');
}
console.log('TLG editor cache, restore keys, and edit events passed for all three fields.');
