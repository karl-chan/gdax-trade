<dashboard-form-stream>
    <form id="dashboard-form-stream">
        <div class="row">
            <div id="dashboard-form-stream-products" class="chips chips-placeholder chips-autocomplete"></div>
        </div>
        <div class="row">
            <p class="grey-text">Select channels</p>
            <div class="col s4" each={channel in config.channels}>
                <input type="checkbox" id="dashboard-form-stream-channel-{channel}" value={channel} checked={selectedChannels.has(channel)}
                    onchange={toggleChannel} class="filled-in" class="filled-in" />
                <label for="dashboard-form-stream-channel-{channel}">{channel}</label>
            </div>
        </div>
        <div class="row">
            <button class="btn waves-effect waves-light teal darken-4 {disabled:!validateSubmit()}" type="submit" name="action">Go
                <i class="material-icons right">send</i>
            </button>
        </div>
    </form>

    <script>
        this.mixin('DashboardController'); // provides controller
        const self = this;

        this.dashboardController.on('streamRequest', (products, channels) => {
            this.fireStreamRequest(products, channels);
        });

        this.selectedProducts = [];
        this.selectedChannels = new Set(['full']);

        this.on('mount', () => {
            // Bind jquery to form submit
            $('#dashboard-form-stream').submit((e) => {
                if (!this.validateSubmit()) {
                    return false;
                }
                const products = this.selectedProducts;
                const channels = Array.from(this.selectedChannels);
                this.fireStreamRequest(products, channels);
                e.preventDefault();
            });

            // Need to manually register chips autocomplete for stream products
            $('#dashboard-form-stream-products').material_chip({
                placeholder: 'Enter a product',
                autocompleteOptions: {
                    data: self.allProductsForAutocomplete(),
                    minLength: 0
                }
            });
            $('#dashboard-form-stream-products').on('chip.add', (e, chip) => {
                this.selectedProducts.push(chip.tag);
                this.update();
            });
            $('#dashboard-form-stream-products').on('chip.delete', (e, chip) => {
                const index = this.selectedProducts.indexOf(chip.tag);
                this.selectedProducts.splice(index, 1);
                this.update();
            });
        });

        toggleChannel(e) {
            const channel = e.target.value;
            if (e.target.checked) {
                this.selectedChannels.add(channel);
            } else {
                this.selectedChannels.delete(channel);
            }
        }

        validateProducts() {
            return this.selectedProducts.length > 0;
        };

        validateChannels() {
            return this.selectedChannels.size > 0;
        }

        validateSubmit() {
            return this.validateProducts() && this.validateChannels();
        }

        fireStreamRequest(products, channels) {
            $.ajax({
                type: 'POST',
                url: '/api/stream',
                success: (response) => {
                    const { signature, key, passphrase, timestamp } = response.auth;
                    const payload = {
                        type: 'subscribe',
                        product_ids: products,
                        channels: channels,
                        signature: signature,
                        key: key,
                        passphrase: passphrase,
                        timestamp: timestamp
                    };
                    const socket = new WebSocket(response.endpoint);
                    socket.onopen = event => socket.send(JSON.stringify(payload));
                    self.handleStreamResponse(undefined, products, channels, socket);
                },
                error: (xhr, ajaxOptions, thrownError) => {
                    const error = `${xhr.status} ${xhr.responseText}`;
                    self.handleStreamResponse(error, products, channels);
                }
            });
        }

        handleStreamResponse(err, products, channels, socket) {
            this.dashboardController.trigger('streamResponse', err, products, channels, socket);
        }

        allProductsForAutocomplete() {
            const products = {}
            for (let p of this.config.products) {
                products[p] = null;
            }
            return products;
        }
    </script>

</dashboard-form-stream>