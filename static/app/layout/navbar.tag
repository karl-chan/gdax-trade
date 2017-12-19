<navbar>
    <nav class="nav-wrapper orange">
        <div class="container">
            <a href="#!" id="drawer-toggle" class="brand-logo" data-activates="drawer">
                <i class="material-icons">menu</i>
                {config.title}
            </a>
            <ul class="right hide-on-med-and-down" no-reorder>
                <li each={config.routes}>
                    <a href={href} class={class} target={target}>
                        <i class="material-icons left">{icon}</i>{label}</a>
                </li>
            </ul>
        </div>
    </nav>
</navbar>