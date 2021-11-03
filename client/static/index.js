require('./styles/main.scss');

class ScrollableContainer extends HTMLElement {
  constructor() {
    super();
  }

  static get observedAttributes() {
    return [];
  }

  connectedCallback() {
    var scrollBarWidth = this.offsetHeight - this.clientHeight;
    this.style.bottom = (-scrollBarWidth) + 'px';

    var parent = this.parentElement;
    while (parent && !parent.classList.contains("scrollable-parent")) {
      parent = parent.parentElement;
    }
    if (parent) {
      parent.style.paddingBottom = scrollBarWidth + 'px';
      this.parentContainer = parent;
    }
  }

  disconnectedCallback() {
    if (this.parentContainer) {
      this.parentContainer.style.paddingBottom = null;
    }
  }

  attributeChangedCallback(name, oldValue, newValue) {
  }
}

customElements.define('scrollable-container', ScrollableContainer);

class DropdownList extends HTMLElement {
  constructor() {
    super();
  }

  static get observedAttributes() {
    return ['selected-option-classes', 'unselected-option-classes', 'selected-value'];
  }

  filterOptions() {
    var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
    options.forEach(o => o.classList.add("hidden-all"));

    var regex = new RegExp(this.input.value, 'i');

    var filteredOptions = options.filter(o => regex.test(o.textContent));
    filteredOptions.forEach(o => o.classList.remove("hidden-all"));

    if (filteredOptions.length == 0) {
      this.optionsNotFound.classList.remove("hidden-all");
    } else {
      this.optionsNotFound.classList.add("hidden-all");
    }
  }

  selectOption(option) {
    this.input.value = this.oldInputValue = "";
    this.filterOptions();
    this.selectedOptionDisplay.innerHTML = option.innerHTML;
    this.selectedOptionDisplay.classList.remove("hidden-all");
    this.optionsParent.classList.add("hidden-visually");

    if (this.selectedOptionClasses && this.unselectedOptionClasses) {
      var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
      options.forEach(o => {
        o.classList.add(...this.unselectedOptionClasses);
        o.classList.remove(...this.selectedOptionClasses);
      });

      var selectedOption = this.querySelector('.options-selectable[data-value="' + this.selectedValue + '"]');
      if (selectedOption) {
        selectedOption.classList.remove(...this.unselectedOptionClasses);
        selectedOption.classList.add(...this.selectedOptionClasses);
      }
    }

    this.dispatchEvent(new CustomEvent('dropdown-list-option-selected', { detail: option.attributes['data-value'].value }));
  }

  connectedCallback() {
    this.input = this.querySelector('input.options-input');
    if (!this.input) {
      throw "DropdownList error: options-input not found!";
    }

    this.optionsNotFound = this.querySelector('ul.options-parent > li.options-not-found');
    if (!this.optionsNotFound) {
      throw "DropdownList error: options-not-found not found!";
    }

    this.optionsParent = this.querySelector('ul.options-parent');
    if (!this.optionsParent) {
      throw "DropdownList error: options-parent not found!";
    }

    this.selectedOptionDisplay = this.querySelector('.options-selected-display');
    if (!this.selectedOptionDisplay) {
      throw "DropdownList error: options-selected-display not found!";
    }

    this.oldInputValue = this.input.value;

    this.selectedOptionClasses = this.attributes['selected-option-classes'] && this.attributes['selected-option-classes'].value.split(' ');
    this.unselectedOptionClasses = this.attributes['unselected-option-classes'] && this.attributes['unselected-option-classes'].value.split(' ');
    this.selectedValue = this.attributes['selected-value'] && this.attributes['selected-value'].value;

    if (this.selectedOptionClasses && this.unselectedOptionClasses) {
      var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
      options.forEach(o => {
        o.classList.add(...this.unselectedOptionClasses);
        o.classList.remove(...this.selectedOptionClasses);
      });

      var selectedOption = this.querySelector('.options-selectable[data-value="' + this.selectedValue + '"]')
      if (selectedOption) {
        this.selectedOptionDisplay.innerHTML = selectedOption.innerHTML;
        selectedOption.classList.remove(...this.unselectedOptionClasses);
        selectedOption.classList.add(...this.selectedOptionClasses);
      } else {
        this.selectedOptionDisplay.innerHTML = "";
      }
    }

    this.addEventListener("click", e => {
      this.input.focus();
    });

    this.input.addEventListener("focus", e => {
      this.selectedOptionDisplay.classList.add("hidden-all");
      this.optionsParent.classList.remove("hidden-visually");
    });

    this.addEventListener('keydown', e => {
      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();

          this.optionsParent.classList.remove("hidden-visually");
          this.selectedOptionDisplay.classList.add("hidden-all");

          var filteredOptions = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable:not(.hidden-all)'));
          if (!filteredOptions.length) {
            break;
          }

          var focusedOptionIndex = filteredOptions.indexOf(document.activeElement);

          if (focusedOptionIndex == -1) {
            var selectedOption = this.querySelector('.options-selectable[data-value="' + this.selectedValue + '"]')
            if (selectedOption) {
              var selectedOptionIndex = filteredOptions.indexOf(selectedOption);
              if (selectedOptionIndex > -1 && selectedOptionIndex < filteredOptions.length - 1) {
                filteredOptions[selectedOptionIndex + 1].focus();
              }
            } else {
              filteredOptions[0].focus();
            }
          } else if (focusedOptionIndex < filteredOptions.length - 1) {
            filteredOptions[focusedOptionIndex + 1].focus();
          }

          break;

        case 'ArrowUp':
          e.preventDefault();

          this.optionsParent.classList.remove("hidden-visually");
          this.selectedOptionDisplay.classList.add("hidden-all");

          var filteredOptions = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable:not(.hidden-all)'));

          if (!filteredOptions.length) {
            break;
          }

          var focusedOptionIndex = filteredOptions.indexOf(document.activeElement);

          if (focusedOptionIndex == -1) {
            var selectedOption = this.querySelector('.options-selectable[data-value="' + this.selectedValue + '"]')
            if (selectedOption) {
              var selectedOptionIndex = filteredOptions.indexOf(selectedOption);
              if (selectedOptionIndex > 0) {
                filteredOptions[selectedOptionIndex - 1].focus();
              }
            } else {
              break;
            }
          } else if (focusedOptionIndex > 0) {
            filteredOptions[focusedOptionIndex - 1].focus();
          } else if (focusedOptionIndex == 0) {
            this.input.focus();
          }

          break;

        case 'Escape':
          this.optionsParent.classList.add("hidden-visually");
          this.selectedOptionDisplay.classList.remove("hidden-all");

          if (this.selectedOptionClasses && this.unselectedOptionClasses) {
            var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
            options.forEach(o => {
              o.classList.add(...this.unselectedOptionClasses);
              o.classList.remove(...this.selectedOptionClasses);
            });

            var selectedOption = this.querySelector('.options-selectable[data-value="' + this.selectedValue + '"]')
            if (selectedOption) {
              selectedOption.classList.remove(...this.unselectedOptionClasses);
              selectedOption.classList.add(...this.selectedOptionClasses);
            }
          }
          break;

        case 'Enter':
          var filteredOptions = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable:not(.hidden-all)'));

          var focusedOptionIndex = filteredOptions.indexOf(document.activeElement);
          if (focusedOptionIndex > -1) {
            this.selectOption(document.activeElement);
          }

        case 'Tab':
          break;

        default:
          if (document.activeElement !== this.input) {
            this.input.focus();
          }

          this.optionsParent.classList.remove("hidden-visually");
      }
    })

    this.addEventListener('keyup', e => {
      switch (e.key) {
        case 'ArrowUp':
        case 'ArrowDown':
        case 'Enter':
        case 'Tab':
        case 'Escape':
          break;

        default:
          if (this.input.value !== this.oldInputValue) {
            this.oldInputValue = this.input.value;
            this.filterOptions();
          }
      }
    });

    var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
    if (this.optionsRemoveListeners && this.optionsRemoveListeners.length) {
      this.optionsRemoveListeners.forEach(remove => remove());
    }
    this.optionsRemoveListeners = [];
    options.forEach(o => {
      var eventListener = e => {
        e.stopPropagation();
        this.selectOption(o);
        this.optionsParent.classList.add("hidden-visually");
      };
      o.addEventListener("click", eventListener);
      this.optionsRemoveListeners.push(() => o.removeEventListener("click", eventListener));
    });

    this.addEventListener('focusin', e => {
      this.classList.remove("z-0");
      this.classList.add("z-10");

      if (this.selectedOptionClasses && this.unselectedOptionClasses) {
        var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
        options.forEach(o => {
          o.classList.add(...this.unselectedOptionClasses);
          o.classList.remove(...this.selectedOptionClasses);
        });

        var selectedOption = this.querySelector('.options-selectable[data-value="' + this.selectedValue + '"]')
        if (selectedOption) {
          if (document.activeElement === selectedOption || document.activeElement === this.input) {
            selectedOption.classList.remove(...this.unselectedOptionClasses);
            selectedOption.classList.add(...this.selectedOptionClasses);
          } else {
            var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
            if (options.indexOf(document.activeElement) > -1) {
              selectedOption.classList.remove(...this.selectedOptionClasses);
              selectedOption.classList.add(...this.unselectedOptionClasses);
            }
          }
        }
      }
    });

    this.addEventListener('focusout', e => {
      setTimeout(() => {
        if (this.clicking) {
          return;
        }

        var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
        if (document.activeElement !== this.input && !options.some(o => document.activeElement === o)) {
          this.classList.remove("z-10");
          this.classList.add("z-0");
          this.optionsParent.classList.add("hidden-visually");
          this.selectedOptionDisplay.classList.remove("hidden-all");
          if (this.selectedOptionClasses && this.unselectedOptionClasses) {
            var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
            options.forEach(o => {
              o.classList.add(...this.unselectedOptionClasses);
              o.classList.remove(...this.selectedOptionClasses);
            });

            var selectedOption = this.querySelector('.options-selectable[data-value="' + this.selectedValue + '"]')
            if (selectedOption) {
              selectedOption.classList.remove(...this.unselectedOptionClasses);
              selectedOption.classList.add(...this.selectedOptionClasses);
            }
          }
        }
      }, 0);
    });

    this.addEventListener("mousedown", e => {
      this.clicking = true;
    });

    this.addEventListener("mouseup", e => {
      this.clicking = false;
    });
  }

  attributeChangedCallback(name, oldValue, newValue) {
    setTimeout(() => {
      this.input = this.querySelector('input.options-input');
      if (!this.input) {
        throw "DropdownList error: options-input not found!";
      }

      this.optionsNotFound = this.querySelector('ul.options-parent > li.options-not-found');
      if (!this.optionsNotFound) {
        throw "DropdownList error: options-not-found not found!";
      }

      this.optionsParent = this.querySelector('ul.options-parent');
      if (!this.optionsParent) {
        throw "DropdownList error: options-parent not found!";
      }

      this.selectedOptionDisplay = this.querySelector('.options-selected-display');
      if (!this.selectedOptionDisplay) {
        throw "DropdownList error: options-selected-display not found!";
      }

      var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
      if (this.optionsRemoveListeners && this.optionsRemoveListeners.length) {
        this.optionsRemoveListeners.forEach(remove => remove());
      }
      this.optionsRemoveListeners = [];
      options.forEach(o => {
        var eventListener = e => {
          e.stopPropagation();
          this.selectOption(o);
          this.optionsParent.classList.add("hidden-visually");
        };
        o.addEventListener("click", eventListener);
        this.optionsRemoveListeners.push(() => o.removeEventListener("click", eventListener));
      });

      this.selectedOptionClasses = this.attributes['selected-option-classes'] && this.attributes['selected-option-classes'].value.split(' ');
      this.unselectedOptionClasses = this.attributes['unselected-option-classes'] && this.attributes['unselected-option-classes'].value.split(' ');
      this.selectedValue = this.attributes['selected-value'] && this.attributes['selected-value'].value;
      if (this.selectedOptionClasses && this.unselectedOptionClasses) {
        var options = Array.from(this.querySelectorAll('ul.options-parent > li.options-selectable'));
        options.forEach(o => {
          o.classList.add(...this.unselectedOptionClasses);
          o.classList.remove(...this.selectedOptionClasses);
        });

        var selectedOption = this.querySelector('.options-selectable[data-value="' + this.selectedValue + '"]')
        if (selectedOption) {
          this.selectedOptionDisplay.innerHTML = selectedOption.innerHTML;
          selectedOption.classList.remove(...this.unselectedOptionClasses);
          selectedOption.classList.add(...this.selectedOptionClasses);
        } else {
          this.selectedOptionDisplay.innerHTML = "";
        }
      }
    }, 0);
  }
}

customElements.define('dropdown-list', DropdownList);

var storedData = localStorage.getItem('symptrack-data');
var flags = storedData ? JSON.parse(storedData) : null;
console.log(flags);

var { Elm } = require('../src/Main');
var app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: flags
});

app.ports.setUserData.subscribe(function (state) {
  localStorage.setItem('symptrack-data', JSON.stringify(state));
});