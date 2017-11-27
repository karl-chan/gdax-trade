<drawer>
    <ul id="drawer" class="side-nav">
        <li>
            <a class="subheader">Menu</a>
        </li>
        <li each={config.routes} no-reorder>
            <a href={href} class={class}>
                <i class="material-icons">{icon}</i>{label}
            </a>
        </li>
    </ul>

    <script>
        this.on('mount', () => {
            $('#drawer-toggle').sideNav();
        });
    </script>
</drawer>