
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
customElements.define('ui-modal', UIModal);

