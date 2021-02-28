
class UIModal extends HTMLElement {
    connectedCallback() {
    }
    static get observedAttributes() { return ['show'] }
    attributeChangedCallback(name, old, value) {

        switch (name) {
        case 'show':
            const arg = value ? 'show' : 'hide'
            $(this).modal({
                detachable: false,
                onHide: () => {
                    this.dispatchEvent(new Event('hide'))
                }
            }).modal(arg)
            break
        }
    }
}

customElements.define('ui-modal', UIModal)

class UIDropdown extends HTMLElement {
    connectedCallback() {
        $(this).children('input').on('change', e => {
            e.stopPropagation()
        })
        $(this).dropdown({
            onChange: value => {
                this.waitCount++
                setTimeout(() => {
                    this.waitCount--
                    if (this.waitCount == 0) {
                        this.value = value
                        if (!this.assigned)
                            this.dispatchEvent(new Event("change"))
                        this.assigned = false
                    }
                }, 10)
            }
        })
        this.value = ""
        this.waitCount = 0;

        this.attributeChangedCallback('value', null, this.getAttribute('value'))
    }

    static get observedAttributes() { return ['value']}
    attributeChangedCallback(name, old, value) {
        switch (name) {
        case 'value':
            if (this.value === undefined) return
            let values = value.split(',')
            if (value != this.value) {
                if (values[0] == '') values = []
                $(this).dropdown('set exactly', values)
            }

            this.value = value
            this.assigned = true

            setTimeout(() => { this.assigned = false }, 20)
            break
        }
    }
}


customElements.define('ui-dropdown', UIDropdown)
