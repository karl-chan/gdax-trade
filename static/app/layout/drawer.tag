<drawer>
    <ul id="drawer" class="side-nav">
        <li>
            <a class="subheader">Menu</a>
        </li>
        <virtual if={config}>
            <li each={config.routes} no-reorder>
                <a href={href} class={class} target={target}>
                    <i class="material-icons">{icon}</i>{label}
                </a>
            </li>
        </virtual>
    </ul>

    <script>
        const self = this;
        self.config = undefined;

        this.on('mount', () => {
            $('#drawer-toggle').sideNav();
        });

        RiotControl.on('config_changed', (config) => {
            self.config = config;
        });
    </script>
</drawer>