function validateField(fieldId, isRequired, min, max, isNumber) {
  const field = $(`#${fieldId}`);
  const value = field.val();
  const validationDiv = $(`#${fieldId}_validation`);
  const label = $(`label[for='${fieldId}']`);

  // Reset validation state
  field.removeClass('is-invalid');
  label.removeClass('text-danger');
  validationDiv.hide();

  // Check if required
  if (isRequired && (value === null || value === '')) {
    setInvalid(field, label, validationDiv, 'This field is required.');
    return false;
  }

  // Check min and max for number inputs
  if (isNumber && value !== '') {
    const numValue = Number.parseFloat(value);
    if (min !== null && numValue < min) {
      setInvalid(field, label, validationDiv, `Value must be at least ${min}.`);
      return false;
    }
    if (max !== null && numValue > max) {
      setInvalid(field, label, validationDiv, `Value must be at most ${max}.`);
      return false;
    }
  }

  return true;
}

function setInvalid(field, label, validationDiv, message) {
  field.addClass('is-invalid');
  label.addClass('text-danger');
  validationDiv.text(message).show();
}
