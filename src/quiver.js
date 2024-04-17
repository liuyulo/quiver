"use strict";

/// A directed n-pseudograph, in which (k + 1)-cells can connect k-cells.
class Quiver {
    constructor() {
        /// An array of array of cells. `cells[k]` is the array of k-cells.
        /// `cells[0]` is therefore the array of objects, etc.
        this.cells = [];

        /// The inter-cell dependencies. That is: the edges that in some way are reliant on this
        /// cell. Each map entry contains a map of edges to their dependency relationship, e.g.
        /// "source" or "target".
        this.dependencies = new Map();

        /// Reverse dependencies (used for removing cells from `dependencies` when removing cells).
        /// That is: cells that this edge is reliant on.
        /// Each map entry is simply a set, unlike `dependencies`.
        this.reverse_dependencies = new Map();

        /// A set of cells that have been deleted. We don't properly delete cells immediately, as
        /// this makes it more awkward to revert deletion (with undo, for instance). Instead we add
        /// them to `deleted` to mark them as such and remove them *solely* from `this.cells`.
        /// Deleted cells are then ignored for all functional purposes involving dependencies.
        /// Though `deleted` is primarily treated as a set, it is really a map from cells to the
        /// point in time (e.g. history state) they were deleted at. This is so we can flush them
        /// later to avoid accumulating memory.
        this.deleted = new Map();
    }

    /// Add a new cell to the graph.
    add(cell) {
        if (!this.deleted.has(cell)) {
            this.dependencies.set(cell, new Map());
            this.reverse_dependencies.set(cell, new Set());

            while (this.cells.length <= cell.level) {
                this.cells.push(new Set());
            }
        } else {
            this.deleted.delete(cell);
        }
        this.cells[cell.level].add(cell);
    }

    /// Remove a cell from the graph.
    remove(cell, when) {
        const removed = new Set();
        const removal_queue = new Set([cell]);
        for (const cell of removal_queue) {
            if (!this.deleted.has(cell)) {
                this.deleted.set(cell, when);
                this.cells[cell.level].delete(cell);
                // The edge case here and below (`|| []`) is for when a cell and its dependencies
                // are being removed simultaneously, in which case the ordering of removal
                // here can cause problems without taking this into consideration.
                for (const [dependency,] of this.dependencies.get(cell) || []) {
                    removal_queue.add(dependency);
                }
                removed.add(cell);
            }
        }
        return removed;
    }

    /// Actually delete all deleted cells from the dependency data structures.
    flush(when) {
        for (const [cell, deleted] of this.deleted) {
            if (deleted >= when) {
                this.dependencies.delete(cell);
                for (const reverse_dependency of this.reverse_dependencies.get(cell) || []) {
                    // If a cell is being removed as a dependency, then some of its
                    // reverse dependencies may no longer exist.
                    if (this.dependencies.has(reverse_dependency)) {
                        this.dependencies.get(reverse_dependency).delete(cell);
                    }
                }
                this.reverse_dependencies.delete(cell);
                this.deleted.delete(cell);
            }
        }
    }

    /// Connect two cells. Note that this does *not* check whether the source and
    /// target are compatible with each other. It does handle reconnecting cells
    /// that may already be connected to other cells.
    connect(source, target, edge) {
        // Clear any existing reverse dependencies. This is necessary if we're
        // reconnecting an edge that is already connected.
        const reverse_dependencies = this.reverse_dependencies.get(edge);
        for (const cell of reverse_dependencies) {
            this.dependencies.get(cell).delete(edge);
        }
        reverse_dependencies.clear();

        this.dependencies.get(source).set(edge, "source");
        this.dependencies.get(target).set(edge, "target");

        reverse_dependencies.add(source);
        reverse_dependencies.add(target);

        [edge.source, edge.target] = [source, target];

        // Reset the cell's (and its dependencies') level to ensure correct spacing when changing
        // the level of the source/target cells.
        for (const cell of this.transitive_dependencies([edge])) {
            const level = Math.max(cell.source.level, cell.target.level) + 1;
            if (this.cells.length < level + 1) {
                this.cells.push(new Set());
            }
            this.cells[cell.level].delete(cell);
            cell.level = level;
            this.cells[level].add(cell);
        }
    }

    /// Returns a collection of all the cells in the quiver.
    all_cells() {
        return Array.from(this.dependencies.keys()).filter((cell) => !this.deleted.has(cell));
    }

    /// Returns whether a cell exists in the quiver (i.e. hasn't been deleted).
    contains_cell(cell) {
        return this.all_cells().includes(cell);
    }

    /// Rerender the entire quiver. This is expensive, so should only be used when more
    /// conservative rerenderings are inappropriate (e.g. when the grid has been resized).
    rerender(ui) {
        const cells = this.all_cells();
        // Sort by level, so that the cells on which others depend are rendered first.
        cells.sort((a, b) => a.level - b.level);
        for (const cell of cells) {
            cell.render(ui);
        }
    }

    /// Returns the `[[x_min, y_min], [x_max, y_max]]` positions of the vertices in the quiver, or
    /// `null` if there are no vertices in the quiver.
    bounding_rect() {
        if (this.is_empty()) {
            return null;
        }

        const vertices = Array.from(this.cells[0]);

        const xs = vertices.map((cell) => cell.position.x);
        const ys = vertices.map((cell) => cell.position.y);

        return [
            [Math.min(...xs), Math.min(...ys)],
            [Math.max(...xs), Math.max(...ys)],
        ];
    }

    /// Returns whether the quiver is empty.
    is_empty() {
        return this.dependencies.size - this.deleted.size === 0;
    }

    /// Returns the non-deleted dependencies of a cell.
    dependencies_of(cell) {
        return new Map(Array.from(this.dependencies.get(cell)).filter(([dependency,]) => {
            return !this.deleted.has(dependency);
        }));
    }

    /// Returns the non-deleted reverse dependencies of a cell.
    reverse_dependencies_of(cell) {
        return new Set(Array.from(this.reverse_dependencies.get(cell)).filter((dependency) => {
            return !this.deleted.has(dependency);
        }));
    }

    /// Returns the transitive closure of the dependencies of a collection of cells
    // (including those cells themselves, unless `exclude_roots`).
    transitive_dependencies(cells, exclude_roots = false) {
        const closure = new Set(cells);
        // We're relying on the iteration order of the `Set` here.
        for (const cell of closure) {
            for (const [dependency,] of this.dependencies.get(cell)) {
                if (!this.deleted.has(dependency)) {
                    closure.add(dependency);
                }
            }
        }
        if (exclude_roots) {
            for (const cell of cells) {
                closure.delete(cell);
            }
        }
        return closure;
    }

    /// Return a `{ data, metadata }` object containing the graph in a specific format.
    /// Currently, the supported formats are:
    /// - "tikz-cd"
    /// - "base64"
    /// - "html"
    /// `settings` describes persistent user settings (like whether to centre the diagram);
    /// `options` describes non-persistent user settings and diagram attributes (like the macro
    /// URL, and the dimensions of the diagram);
    /// `definitions` contains key-value pairs for macros and colours.
    export(format, settings, options, definitions) {
        switch (format) {
            case "tikz-cd":
                return QuiverImportExport.tikz_cd.export(this, settings, options, definitions);
            case "base64":
                return QuiverImportExport.base64.export(this, settings, options, definitions);
            case "html":
                return QuiverExport.html.export(this, settings, options, definitions);
            default:
                throw new Error(`unknown export format \`${format}\``);
        }
    }

    /// Return a `{ data, metadata }` object.
    /// Currently, the supported formats are:
    /// - "tikz-cd"
    /// `settings` describes persistent user settings (like whether to centre the diagram);
    import(ui, format, data, settings) {
        switch (format) {
            case "tikz-cd":
                return QuiverImportExport.tikz_cd.import(ui, data, settings);
            default:
                throw new Error(`unknown export format \`${format}\``);
        }
    }
}

/// Various methods of exporting a quiver.
class QuiverExport {
    /// A method to export a quiver as a string.
    export() { }
}

/// Various methods of exporting and importing a quiver.
class QuiverImportExport extends QuiverExport {
    /// A method to import a quiver as a string. `import(export(quiver))` should be the
    /// identity function. Currently `import` takes a `UI` into which to import directly.
    import() { }

    begin_import(ui) {
        // We don't want to relayout every time we add a new cell: instead, we should perform
        // layout once, once all of the cells have been created.
        ui.buffer_updates = true;
    }

    end_import(ui) {
        // Centre the view on the quiver.
        ui.centre_view();
        // Also centre the focus point, so that it's centre of screen.
        // We subtract 0.5 from the position so that when the view is centred perfectly between
        // two cells, we prefer the top/leftmost cell.
        ui.focus_point.class_list.remove("smooth");
        ui.reposition_focus_point(ui.position_from_offset(ui.view.sub(Point.diag(0.5))));
        ui.focus_point.class_list.add("focused");
        delay(() => ui.focus_point.class_list.add("smooth"));

        // When cells are created, they are usually queued. We don't want any cells that have been
        // imported to be queued.
        for (const cell of ui.quiver.all_cells()) {
            cell.element.query_selector("kbd.queue").class_list.remove("queue");
        }

        // Update all the affected columns and rows.
        delay(() => ui.update_col_row_size(
            ...ui.quiver.all_cells()
                .filter((cell) => cell.is_vertex()).map((vertex) => vertex.position)
        ));

        // Stop buffering updates, so that individual changes to cells will resize the grid.
        ui.buffer_updates = false;

        // If the quiver is now nonempty, some toolbar actions will be available.
        ui.toolbar.update(ui);
        ui.update_focus_tooltip();
    }
}

QuiverExport.CONSTANTS = {
    // For curves and shortening, we need to try to convert proportional measurements
    // into absolute distances (in `pt`) for TikZ. There are several subtleties, one of
    // which is that the grid cell size in tikz-cd has a greater width than height, so
    // when we scale things, we need to scale differently in the horizontal and vertical
    // directions. For now, we simply multiply by constants that, heuristically, give
    // reasonable results for various diagrams I tested. It would be nice to eventually
    // correct this by using proportional lengths, but that requires a custom TikZ style
    // I do not currently possess the skills to create.
    TIKZ_HORIZONTAL_MULTIPLIER: 1 / 4,
    TIKZ_VERTICAL_MULTIPLIER: 1 / 6,
};

QuiverImportExport.tikz_cd = new class extends QuiverImportExport {
    wrap_boilerplate = (output, q, s, o, d) => {
        let diagram = output.map(line => `  ${line},`).join("\n").trim();
        // if nonempty diagram
        if (diagram.length) diagram = `\n  ${diagram}\n`;

        diagram = `#diagram(${diagram})`;
        if (s.get("export.centre_diagram")) {
            diagram = `$ ${diagram} $`;
        }
        const hash = QuiverImportExport.base64.export(q, s, o, d).data;
        if (hash === "") return diagram;
        return `// #q=${hash}\n${diagram}`;
    };

    // arrow building with options
    arrow(o) {
        // refer to the manual for the meanings of the strings M1, M2, M3 and L
        let arrow_M1 = "";
        let arrow_M2 = "";
        let arrow_M3 = "";
        let arrow_L = "";

        switch (o.style.body.name) {
            case "barred":
                arrow_M2 = "|";
            // intentional fallthrough
            case "cell":
                if (o.level == 1) {
                    arrow_L = "-";
                } else if (o.level == 2) {
                    arrow_L = "=";
                } else if (o.level >= 3) {
                    // typst-fletcher only supports up to triple arrows
                    arrow_L = "==";
                }
                break;
            case "squiggly":
            // not supported, falling back to --
            case "dashed":
                arrow_L = "--";
                break;
            case "dotted":
                arrow_L = "..";
                break;
            default:
                // typst-fletcher does not support arrows without body
                arrow_L = "-";
                break;
        }

        switch (o.style.tail.name) {
            case "maps to":
                arrow_M1 = "|";
                break;
            case "mono":
                arrow_M1 = ">";
                break;
            case "hook":
                arrow_M1 = `hook${o.style.tail.side == "top" ? "" : "'"}`;
                break;
            case "arrowhead":
                arrow_M1 = "<";
                break;
        }

        switch (o.style.head.name) {
            case "epi":
                arrow_M3 = ">>";
                break;
            case "harpoon":
                arrow_M3 = `hook${o.style.tail.side == "top" ? "" : "'"}`;
                break;
            case "arrowhead":
                arrow_M3 = ">";
                break;
        }

        const arrow = arrow_M2 === "" ?
            `${arrow_M1}${arrow_L}${arrow_M3}` :
            `${arrow_M1}${arrow_L}${arrow_M2}${arrow_L}${arrow_M3}`;
        return `"${arrow}"`;
    }

    // label to string (with color)
    ltos(label, c) {
        if (label === "") return '" "';
        label = `$${label}$`;
        if (c.is_not_black()) return `text(fill: ${c.typst()}, ${label})`;
        return label;
    }

    // edge params
    params(label, o) {
        const params = {};
        if (label !== "") {
            console.log(o.label_alignment)
            switch (o.label_alignment) {
                case "centre":
                    params["label-side"] = "center";
                    params["label-fill"] = "true";
                    break;
                case "over":
                    params["label-side"] = "center";
                    params["label-fill"] = "false";
                    break;
                case "right":
                    params["label-side"] = "right";
                    break;
            }
            if (o.label_position !== 50) {
                params["label-pos"] = o.label_position / 100;
            }
        }

        if (o.offset !== 0) {
            params["shift"] = `${o.offset}pt`;
        }

        if (o.colour.is_not_black()) {
            params["stroke"] = o.colour.typst();
        }

        if (o.curve !== 0) {
            // temporary
            params["bend"] = `${-15 * o.curve}deg`;
        }

        return params;
    }

    // edge mark
    marks(o) {
        switch (o.style.name) {
            case "arrow":
                return this.arrow(o);
            case "adjunction":
            case "corner":
            case "corner-inverse":
                return `"->"`;
        }
    }

    // quiver, settings, options, definitions
    export(quiver, s, o, d) {
        let output = [];
        const metadata = { tikz_incompatibilities: new Set(), dependencies: new Map() }

        // Early exit for empty quivers.
        if (quiver.is_empty())
            return {
                data: this.wrap_boilerplate(output, quiver, s, o, d),
                metadata,
            };

        // add nodes
        for (const { label, label_colour, position: { x, y } } of quiver.cells[0]) {
            output.push(`node((${x}, ${y}), ${this.ltos(label, label_colour)})`);
        }

        // add edges
        for (let level = 1; level < quiver.cells.length; level++) {
            // TODO k-cells with k >= 2 is currently unsupported
            // See: https://github.com/Jollywatt/typst-fletcher/issues/16
            if (level > 1) {
                break;
            }
            for (const edge of quiver.cells[level]) {
                const { source, target, options: o, label, label_colour } = edge;
                console.assert(source.is_vertex() && target.is_vertex());
                const params = this.params(label, o);
                const marks = this.marks(o);
                const args = [
                    `(${source.position.x}, ${source.position.y})`,
                    `(${target.position.x}, ${target.position.y})`,
                    label === '' ? '' : this.ltos(label, label_colour),
                    marks,
                    ...Object.entries(params).map(([k, v]) => `${k}: ${v}`)
                ]
                output.push(`edge(${args.filter(Boolean).join(", ")})`);
            }
        }

        return {
            data: this.wrap_boilerplate(output, quiver, s, o, d),
            metadata,
        };
    }

    import(ui, data) {
        this.begin_import(ui);

        const parser = new Parser(ui, data);
        parser.parse_diagram();

        this.end_import(ui);

        return { diagnostics: parser.diagnostics };
    }
};

QuiverImportExport.base64 = new class extends QuiverImportExport {
    // The format we use for encoding quivers in base64 (primarily for link-sharing) is
    // the following. This has been chosen based on minimality (for shorter representations),
    // rather than readability.
    //
    // Note that an empty quiver has no representation.
    //
    // `[version: integer, |vertices|: integer, ...vertices, ...edges]`
    //
    // Parameters:
    // - `version` is currently only permitted to be 0. The format has been designed to be
    //   forwards-compatible with changes, so it is intended that this version will not
    //   change.
    // - `|vertices|` is the length of the array `vertices`.
    // - `vertices` is an array of vertices of the form:
    //      `[x: integer, y: integer, label: string, label_colour: [h, s, l, a]]`
    //      + `label` is optional (if not present, it will default to `""`), though it must be
    //         present if any later option is.
    //      + `label_colour` is optional (if not present, it will default to `[0, 0, 0, 1]`).
    //          + `h` is an integer from `0` to `360`
    //          + `s` is an integer from `0` to `100`
    //          + `l` is an integer from `0` to `100`
    //          + `a` is a floating-point number from `0` to `1`
    // - `edges` is an array of edges of the form:
    //      `[source: index, target: index, label: string, alignment, options, label_colour]`
    //      + (label) `alignment` is an enum comprising the following options:
    //          * `0`: left
    //          * `1`: centre
    //          * `2`: right
    //          * `3`: over
    //        It has been distinguished from the other options as one that is frequently
    //        changed from the default, to avoid the overhead of encoding an options
    //        object.
    //      + `options` is an object containing the delta of the options from the defaults.
    //         This is the only parameter that is not encoded simply as an array, as the
    //         most likely parameter to be changed in the future.
    //      + `label_colour` is stored in the same manner as for vertices.
    //
    // Notes:
    // - An `index` is an integer indexing into the array `[...vertices, ...edges]`.
    // - Arrays may be truncated if the values of the elements are the default values.

    export(quiver, _, options) {
        if (quiver.is_empty()) {
            // No need to have an encoding of an empty quiver;
            // we'll just use the URL directly.
            return {
                data: "",
                metadata: {},
            };
        }

        const cells = [];
        const indices = new Map();

        let offset = new Position(Infinity, Infinity);
        // We want to ensure that the top-left cell is in position (0, 0), so we need
        // to work out where the top-left cell actually is, to compute an offset.
        for (const vertex of quiver.cells[0]) {
            offset = offset.min(vertex.position);
        }
        for (const vertex of quiver.cells[0]) {
            const { label, label_colour } = vertex;
            indices.set(vertex, cells.length);
            const position = vertex.position.sub(offset).toArray();
            const cell = [...position];
            // In the name of efficiency, we omit any parameter that is not necessary, and for which
            // no later parameter is necessary.
            if (label !== "") {
                cell.push(label);
            }
            if (label !== "" && label_colour.is_not_black()) {
                // Even if the colour is not black, it's irrelevant if there is no label.
                cell.push(label_colour.hsla());
            }
            cells.push(cell);
        }

        for (let level = 1; level < quiver.cells.length; ++level) {
            for (const edge of quiver.cells[level]) {
                const { label, label_colour, options: { label_alignment, ...options } } = edge;
                const [source, target] = [indices.get(edge.source), indices.get(edge.target)];
                indices.set(edge, cells.length);
                const cell = [source, target];
                // We want to omit parameters that are unnecessary (i.e. have the default
                // values). However, because we store parameters in an array, the only way
                // we can distinguish missing values is by the length. Therefore, we can
                // only truncate the array (not omit elements partway through the array).
                // This means we may need to include unnecessary information if there is a
                // non-default parameter after a default one. The parameters most likely to
                // be default are placed further back in the array to reduce the frequency
                // of this situation.
                const end = [];

                // Even if the colour is not black, it's irrelevant if there is no label.
                if (label !== "" && label_colour.is_not_black()) {
                    end.push(label_colour.hsla());
                }

                // We compute a delta of the edge options compared
                // to the default, so we encode a minimum of data.
                const default_options = Edge.default_options({ level });

                // Recursively compute a delta between an `object` and `base`.
                const probe = (object, base) => {
                    const delta = {};
                    for (const [key, value] of Object.entries(object)) {
                        const default_value = base[key];
                        if (default_value instanceof Encodable && value instanceof Encodable) {
                            if (!default_value.eq(value)) {
                                delta[key] = value;
                            }
                        } else if (typeof default_value === "object" && typeof value === "object") {
                            const subdelta = probe(value, default_value);
                            if (Object.keys(subdelta).length > 0) {
                                delta[key] = subdelta;
                            }
                        } else if (default_value !== value) {
                            delta[key] = value;
                        }
                    }
                    return delta;
                };

                const delta = probe(options, default_options);

                // Some parameters are redundant and are used only for convenience, so we strip them
                // out.
                delete delta["shape"];
                switch (edge.options.shape) {
                    case "bezier":
                        delete delta["radius"];
                        delete delta["angle"];
                        break;
                    case "arc":
                        delete delta["curve"];
                        break;
                }

                if (end.length > 0 || Object.keys(delta).length > 0) {
                    end.push(delta);
                }

                const push_if_necessary = (parameter, default_value, condition = true) => {
                    if (end.length > 0 || (parameter !== default_value && condition)) {
                        end.push(parameter);
                    }
                };

                const variant = { left: 0, centre: 1, right: 2, over: 3 }[label_alignment];
                // It's only necessary to encode the label alignment if the label is not blank.
                push_if_necessary(variant, 0, label !== "");
                push_if_necessary(label, "");

                cell.push(...end.reverse());
                cells.push(cell);
            }
        }

        // The version of the base64 output format exported by this version of quiver.
        const VERSION = 0;
        const output = [VERSION, quiver.cells[0].size, ...cells];

        const encoder = new TextEncoder();
        return {
            data: btoa(String.fromCharCode(...encoder.encode(JSON.stringify(output)))),
            metadata: {},
        };
    }

    import(ui, string) {
        let input;
        try {
            const data = atob(string);
            const bytes = [];
            for (let i = 0; i < data.length; ++i) {
                bytes.push(data.charCodeAt(i));
            }
            const decoded = new TextDecoder().decode(new Uint8Array(bytes));
            if (decoded === "") {
                return;
            }
            input = JSON.parse(decoded);
        } catch (_) {
            throw new Error("invalid base64 or JSON");
        }

        // Helper functions for dealing with bad input.

        const assert = (condition, message) => {
            const postfix = " in quiver encoding";
            if (!condition) {
                throw new Error(`${message}${postfix}`);
            }
        };
        const assert_kind = (object, kind) => {
            switch (kind) {
                case "array":
                    assert(Array.isArray(object), "expected array");
                    break;
                case "integer":
                case "natural":
                    assert(Number.isInteger(object), "expected integer");
                    if (kind === "natural") {
                        assert(object >= 0, "expected non-negative integer");
                    }
                    break;
                case "float":
                    assert(typeof object === "number", "expected floating-point number");
                    break;
                case "boolean":
                    assert(typeof object === "boolean", "expected boolean");
                    break;
                case "string":
                    assert(typeof object === "string", "expected string");
                    break;
                case "object":
                    assert(typeof object === "object", "expected object");
                    break;
                case "colour":
                    assert_kind(object, "array");
                    assert(object.length >= 3 && object.length <= 4, "invalid colour format");
                    const [h, s, l, a = 1] = object;
                    assert_kind(h, "natural");
                    assert(h <= 360, "invalid hue");
                    assert_kind(s, "natural");
                    assert(s <= 100, "invalid saturation");
                    assert_kind(l, "natural");
                    assert(l <= 100, "invalid lightness");
                    assert_kind(a, "float");
                    assert(a >= 0 && a <= 1, "invalid alpha");
                    break;
                default:
                    throw new Error(`unknown parameter kind \`${kind}\``);
            }
        };
        const assert_eq = (object, value) => {
            assert(object === value, `expected \`${value}\`, but found \`${object}\``);
        };

        // Check all of the non-cell data is valid.
        assert_kind(input, "array");
        const [version = 0, vertices = 0, ...cells] = input;
        assert_kind(version, "natural");
        assert_eq(version, 0);
        assert_kind(vertices, "natural");
        assert(vertices <= cells.length, "invalid number of vertices");

        // If we encounter errors while loading cells, we skip the malformed cell and try to
        // continue loading the diagram, but we want to report the errors we encountered afterwards,
        // to let the user know we were not entirely successful.
        const errors = [];

        this.begin_import(ui);

        const indices = [];
        for (const cell of cells) {
            try {
                assert_kind(cell, "array");

                if (indices.length < vertices) {
                    // This cell is a vertex.

                    assert(cell.length >= 2 && cell.length <= 4, "invalid vertex format");
                    const [x, y, label = "", label_colour = Colour.black().hsla()] = cell;
                    assert_kind(x, "natural");
                    assert_kind(y, "natural");
                    assert_kind(label, "string");
                    assert_kind(label_colour, "colour");

                    const vertex = new Vertex(
                        ui,
                        label,
                        new Position(x, y),
                        new Colour(...label_colour),
                    );
                    indices.push(vertex);
                } else {
                    // This cell is an edge.

                    assert(cell.length >= 2 && cell.length <= 6, "invalid edge format");
                    const [
                        source, target, label = "", alignment = 0, options = {},
                        label_colour = Colour.black().hsla()
                    ] = cell;
                    for (const [endpoint, name] of [[source, "source"], [target, "target"]]) {
                        assert_kind(endpoint, "natural");
                        assert(endpoint < indices.length, `invalid ${name} index`);
                    }
                    assert_kind(label, "string");
                    assert_kind(alignment, "natural");
                    assert(alignment <= 3, "invalid label alignment");
                    assert_kind(options, "object");
                    assert_kind(label_colour, "colour");

                    // We don't restrict the keys on `options`, because it is likely that `options`
                    // will be extended in the future, and this permits a limited form of backwards
                    // compatibility. We never access prototype properties on `options`, so this
                    // should not be amenable to injection. However, for those properties we do
                    // expect to exist, we do check they have the correct type (and in some cases,
                    // range), below.

                    let level = Math.max(indices[source].level, indices[target].level) + 1;
                    const { style = {} } = options;
                    delete options.style;

                    // Validate `options`.
                    if (options.hasOwnProperty("label_position")) {
                        assert_kind(options.label_position, "natural");
                        assert(options.label_position <= 100, "invalid label position");
                    }
                    if (options.hasOwnProperty("offset")) {
                        assert_kind(options.offset, "integer");
                    }
                    if (options.hasOwnProperty("curve")) {
                        assert_kind(options.curve, "integer");
                    }
                    if (options.hasOwnProperty("radius")) {
                        assert_kind(options.radius, "integer");
                    }
                    if (options.hasOwnProperty("angle")) {
                        assert_kind(options.angle, "integer");
                    }
                    if (options.hasOwnProperty("shorten")) {
                        let shorten = { source: 0, target: 0 };
                        if (options.shorten.hasOwnProperty("source")) {
                            assert_kind(options.shorten.source, "natural");
                            shorten.source = options.shorten.source;
                        }
                        if (options.shorten.hasOwnProperty("target")) {
                            assert_kind(options.shorten.target, "natural");
                            shorten.target = options.shorten.target;
                        }
                        assert(shorten.source + shorten.target <= 100, "invalid shorten");
                    }
                    if (options.hasOwnProperty("colour")) {
                        assert_kind(options.colour, "colour");
                        // Colour is encoded as an array, so we have to convert it to a `Colour`.
                        options.colour = new Colour(...options.colour);
                    }
                    if (options.hasOwnProperty("edge_alignment")) {
                        if (options.edge_alignment.hasOwnProperty("source")) {
                            assert_kind(options.edge_alignment.source, "boolean");
                        }
                        if (options.edge_alignment.hasOwnProperty("target")) {
                            assert_kind(options.edge_alignment.target, "boolean");
                        }
                    }

                    // In previous versions of quiver, there was a single `length` parameter, rather
                    // than two `shorten` parameters. We convert from `length` into `shorten` here.
                    if (options.hasOwnProperty("length")) {
                        assert_kind(options.length, "natural");
                        assert(options.length >= 0 && options.length <= 100, "invalid length");
                        // If both `length` and `shorten` are present (which should not happen for
                        // diagrams exported by quiver), `shorten` takes priority.
                        if (!options.hasOwnProperty("shorten")) {
                            const shorten = 100 - options.length;
                            options.shorten = { source: shorten / 2, target: shorten / 2 };
                        }
                        delete options.length;
                    }

                    // In previous versions of quiver, `level` was only valid for some arrows, and
                    // was recorded in the body style, rather than as a property of the edge itself.
                    // For backwards-compatibility, we check for this case manually here.
                    if (style.hasOwnProperty("body") && style.body.hasOwnProperty("level")) {
                        assert_kind(style.body.level, "natural");
                        assert(style.body.level >= 1, "invalid level");
                        level = style.body.level;
                        delete style.body.level;
                    }

                    const edge = new Edge(
                        ui,
                        label,
                        indices[source],
                        indices[target],
                        Edge.default_options({
                            level,
                            label_alignment: ["left", "centre", "right", "over"][alignment],
                            ...options,
                        }, style),
                        new Colour(...label_colour),
                    );
                    indices.push(edge);
                }
            } catch (error) {
                errors.push(error);
            }
        }

        this.end_import(ui);

        if (errors.length > 0) {
            // Just throw the first error.
            throw errors[0];
        }
    }
};

QuiverExport.html = new class extends QuiverExport {
    export(quiver, settings, options, definitions) {
        const url = QuiverImportExport.base64.export(quiver, settings, options, definitions).data;
        let [width, height] = settings.get("export.embed.fixed_size") ? [
            settings.get("export.embed.width"),
            settings.get("export.embed.height"),
        ] : [
            options.dimensions.width + 2 * CONSTANTS.EMBED_PADDING,
            options.dimensions.height + 2 * CONSTANTS.EMBED_PADDING,
        ];
        return {
            data: `<!-- ${url} -->
<iframe class="quiver-embed" \
src="${url}${!quiver.is_empty() ? "&" : "#"}embed" \
width="${width}" \
height="${height}" \
style="border-radius: 8px; border: none;">\
</iframe>`,
            metadata: {},
        };
    }
};
