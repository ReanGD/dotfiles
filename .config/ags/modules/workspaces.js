const hyprland = await Service.import("hyprland")

function makeWorkspaceItem(workspace, activeId) {
    var class_name = ""
    if (activeId === workspace.id) {
        class_name += " focused"
    }
    if (workspace.windows > 0) {
        class_name += " occupied"
    }

    return Widget.Button({
        cursor: "pointer",
        class_name: class_name,
        child: Widget.Label(workspace.name),
        onClicked: () => hyprland.messageAsync(`dispatch workspace ${workspace.id}`),
    })
}

export default () => {
    const view = Widget.Box({
        class_name: "workspaces",
    })

    function updateWorkspaces() {
        const activeId = hyprland.active.workspace.id
        const workspaces = hyprland.workspaces
        view.children = workspaces
            .sort((a, b) => a.id - b.id)
            .map(workspace => makeWorkspaceItem(workspace, activeId))
    }

    return Widget.EventBox({
        child: view,
        setup: (self) => {
            updateWorkspaces()
            self.hook(hyprland.active.workspace, updateWorkspaces, "changed")
            self.hook(hyprland, updateWorkspaces, "notify::workspaces")
        }
    })
}
