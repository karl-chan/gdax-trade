<navbar>
    <nav class="nav-wrapper orange">
        <div class="container">
            <a href="#!" id="drawer-toggle" class="brand-logo" data-activates="drawer">
                <i class="material-icons">menu</i>
                <span if={config}>{config.title}</span>
            </a>
            <ul if={config} class="right hide-on-med-and-down" no-reorder>
                <li each={config.routes}>
                    <a href={href} class={class} target={target}>
                        <i class="material-icons left">{icon}</i>{label}</a>
                </li>
            </ul>
        </div>
    </nav>

    <script>
        const self = this;
        self.config = undefined;

        console.log(RiotControl)

        RiotControl.on('config_update', (config) => {
            console.log('hello')
            self.config = config;
        })
    </script>
</navbar>