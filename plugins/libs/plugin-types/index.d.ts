/**
 * These are methods and properties available on the `penpot` global object.
 *
 */
export interface Penpot extends Omit<
  Context,
  'addListener' | 'removeListener' | 'management'
> {
  readonly ui: {
    /**
     * Opens the plugin UI. It is possible to develop a plugin without interface (see Palette color example) but if you need, the way to open this UI is using `penpot.ui.open`.
     * There is a minimum and maximum size for this modal and a default size but it's possible to customize it anyway with the options parameter.
     *
     * @param name title of the plugin, it'll be displayed on the top of the modal
     * @param url of the plugin
     * @param options width and height of the modal and, optionally, a `hidden` flag to open the plugin without showing the modal.
     *
     * @example
     * ```js
     * penpot.ui.open('Plugin name', 'url', {width: 150, height: 300});
     * ```
     */
    open: (
      name: string,
      url: string,
      options?: { width: number; height: number; hidden?: boolean },
    ) => void;

    /**
     * The current size of the modal, or `null` if the plugin UI is not open.
     * @example
     * ```js
     * const size = penpot.ui.size;
     * console.log(size);
     * ```
     */
    size: {
      /**
       * The width of the modal.
       */
      width: number;
      /**
       * The height of the modal.
       */
      height: number;
    } | null;

    /**
     * Resizes the plugin UI.
     * @param width The width of the modal.
     * @param height The height of the modal.
     * @example
     * ```js
     * penpot.ui.resize(300, 400);
     * ```
     * */
    resize: (width: number, height: number) => void;

    /**
     * Sends a message to the plugin UI.
     *
     * @param message content usually is an object
     * @param throwOnError if true, throws an error when the message cannot be cloned instead of logging to console (default: false)
     *
     * @example
     * ```js
     * penpot.ui.sendMessage({ type: 'example-type', content: 'data we want to share' });
     * ```
     */
    sendMessage: (message: unknown, throwOnError?: boolean) => void;
    /**
     * This is usually used in the `plugin.ts` file in order to handle the messages sent from the plugin UI.
     *
     * @param callback - A function that will be called whenever a message is received.
     * The function receives a single argument, `message`, which is of type `T`.
     *
     * @example
     * ```js
     * penpot.ui.onMessage((message) => { if (message.type === 'example-type') { ...do something } });
     * ```
     */
    onMessage: <T>(callback: (message: T) => void) => void;
  };
  /**
   * Provides access to utility functions and context-specific operations.
   */
  readonly utils: ContextUtils;
  /**
   * Closes the plugin. When this method is called the UI will be closed.
   *
   * @example
   * ```js
   * penpot.closePlugin();
   * ```
   */
  closePlugin: () => void;
  /**
   * Adds an event listener for the specified event type.
   * Subscribing to events requires `content:read` permission.
   *
   * The following are the possible event types:
   *  - pagechange: event emitted when the current page changes. The callback will receive the new page.
   *  - shapechange: event emitted when the shape changes. This event requires to send inside the `props` object the shape
   *  that will be observed. For example:
   *  ```javascript
   *  // Observe the current selected shape
   *  penpot.on('shapechange', (shape) => console.log(shape.name), { shapeId: penpot.selection[0].id });
   *  ```
   *  - selectionchange: event emitted when the current selection changes. The callback will receive the list of ids for the new selection
   *  - themechange: event emitted when the user changes its theme. The callback will receive the new theme (currently: either `dark` or `light`)
   *  - filechange: event emitted when a different file is opened. The callback will receive the new file.
   *  - contentsave: event emitted after the file content is saved in the backend. The callback receives no arguments.
   *  - finish: event emitted when the current file is closed. The callback will receive the id of the closed file.
   *
   * @param type The event type to listen for.
   * @param callback The callback function to execute when the event is triggered.
   * @param props The properties for the current event handler. Only makes sense for specific events.
   * @return the listener id that can be used to call `off` and cancel the listener
   *
   * @example
   * ```js
   * penpot.on('pagechange', () => {...do something});
   * ```
   */
  on<T extends keyof EventsMap>(
    type: T,
    callback: (event: EventsMap[T]) => void,
    props?: { [key: string]: unknown },
  ): symbol;

  /**
   * Removes an event listener for the specified event type.
   *
   * @param listenerId the id returned by the `on` method when the callback was set
   *
   * @example
   * ```js
   * const listenerId = penpot.on('contentsave', () => console.log("Changed"));
   * penpot.off(listenerId);
   * ```
   */
  off(listenerId: symbol): void;
}

/**
 * Type for all the possible types of actions in an interaction.
 */
export type Action =
  | NavigateTo
  | OpenOverlay
  | ToggleOverlay
  | CloseOverlay
  | PreviousScreen
  | OpenUrl
  | PlayToMarker;

/**
 * Represents an active user in Penpot, extending the `User` interface.
 * This interface includes additional properties specific to active users.
 */
export interface ActiveUser extends User {
  /**
   * The position of the active user.
   *
   * @example
   * ```js
   * const userPosition = activeUser.position;
   * console.log(userPosition);
   * ```
   */
  position?: { x: number; y: number };
  /**
   * The zoom level of the active user.
   *
   * @example
   * ```js
   * const userZoom = activeUser.zoom;
   * console.log(userZoom);
   * ```
   */
  readonly zoom?: number;
}

/**
 * Type of all the animations that can be added to an interaction.
 */
export type Animation = Dissolve | Slide | Push | Flip | Cube;

/**
 * Represents blur properties in Penpot.
 * This interface includes properties for defining the type and intensity of a blur effect, along with its visibility.
 */
export interface Blur {
  /**
   * The optional unique identifier for the blur effect.
   */
  id?: string;
  /**
   * The optional intensity value of the blur effect.
   */
  value?: number;
  /**
   * Specifies whether the blur effect is hidden.
   * Defaults to false if omitted.
   */
  hidden?: boolean;
}

/**
 * The light of the layers in 3D of a board (`Board.light`). Penpot Motion.
 */
export interface Light {
  /**
   * Where the light comes from, degrees round the view, clockwise from
   * its top: 0 from the top, 90 from the right.
   */
  angle: number;
  /**
   * How high the light is, degrees above the board (0 to 90): 90 shines
   * straight at it.
   */
  height: number;
  /**
   * The share of the light (0 to 1) that comes from all round: faces
   * turned away from the light keep this much.
   */
  ambient: number;
  /**
   * When true the drop and inner shadows of the layers of the board fall
   * away from the light, each as long as it is: setting the light turns
   * them, and they turn with it as its angle plays (the keyframe property
   * `lightAngle`).
   */
  shadows?: boolean;
}

/**
 * Represents a board in Penpot.
 * This interface extends `ShapeBase` and includes properties and methods specific to board.
 */
export interface Board extends ShapeBase {
  /**
   * The type of the shape, which is always 'board' for boards.
   */
  readonly type: 'board';

  /**
   * When true the board will clip the children inside
   */
  clipContent: boolean;

  /**
   * When true the board will be displayed in the view mode
   */
  showInViewMode: boolean;

  /**
   * How far the viewer of the tilted layers of the board is, in px
   * (100 to 100000), as CSS `perspective`; their vanishing point is at
   * `perspectiveX`, `perspectiveY`. `null` is the default, 1000.
   * Animatable as the keyframe property `perspective`.
   */
  perspective: number | null;

  /**
   * Where the vanishing point of the tilted layers of the board is across
   * it, a fraction of its width (0 is its left edge, 1 its right one), as
   * CSS `perspective-origin`. `null` is its middle, 0.5. Animatable as the
   * keyframe property `perspectiveX`.
   */
  perspectiveX: number | null;

  /**
   * Where the vanishing point of the tilted layers of the board is down
   * it, a fraction of its height (0 is its top edge, 1 its bottom one).
   * `null` is its middle, 0.5. Animatable as the keyframe property
   * `perspectiveY`.
   */
  perspectiveY: number | null;

  /**
   * When true the board sees its tilted layers without perspective, as
   * from afar (a parallel projection): for isometric drawings, with the
   * layers turned by rotation 60, rotateX 35.2644 and rotateY -45.
   * Penpot Motion.
   */
  orthographic: boolean;

  /**
   * The light of the layers of the board that leave its plane (tilted,
   * moved in depth or in a 3D space): faces turned to it show brighter,
   * faces turned away darker, faces looking at the viewer keep their
   * colours. `null` is no light; a partial object takes the rest from
   * `{ angle: 315, height: 45, ambient: 0.3 }`. Penpot Motion.
   *
   * A light from the right, one from the top left whose shadows fall to
   * the bottom right, and none:
   *
   * @example
   * ```js
   * board.light = { angle: 90, height: 30, ambient: 0.2 };
   * board.light = { angle: 315, shadows: true };
   * board.light = null;
   * ```
   */
  light: Light | null;

  /**
   * Depth cue: the share of their opacity (0 to 1) the layers of the
   * board that leave its plane lose each 1000 px behind `depthFocus`.
   * 0 is none.
   * Penpot Motion.
   */
  depthFade: number;

  /**
   * Depth cue: the px of blur (0 to 100) the layers of the board that
   * leave its plane get each 100 px off `depthFocus`. 0 is none.
   * Penpot Motion.
   */
  depthBlur: number;

  /**
   * Depth cue: the depth that stays sharp and clear, px towards the
   * viewer from the board (negative: behind it): `depthBlur` counts from
   * it, and `depthFade` behind it. Penpot Motion.
   */
  depthFocus: number;

  /**
   * When true the board, unless it clips its content, is a 3D space: its
   * children draw back to front by their depth (see `Group.preserve3d`).
   * Penpot Motion.
   */
  preserve3d: boolean;

  /**
   * The grid layout configuration of the board, if applicable.
   */
  readonly grid?: GridLayout;

  /**
   * The flex layout configuration of the board, if applicable.
   */
  readonly flex?: FlexLayout;

  /**
   * The guides associated with the board.
   */
  guides: Guide[];

  /**
   * The ruler guides attached to the board
   */
  readonly rulerGuides: RulerGuide[];

  /**
   * The horizontal sizing behavior of the board.
   * It can be one of the following values:
   * - 'fix': The container has its own intrinsic fixed size.
   * - 'auto': The container fits the content.
   */
  horizontalSizing?: 'auto' | 'fix';

  /**
   * The vertical sizing behavior of the board.
   * It can be one of the following values:
   * - 'fix': The container has its own intrinsic fixed size.
   * - 'auto': The container fits the content.
   */
  verticalSizing?: 'auto' | 'fix';

  /**
   * The fills applied to the shape.
   */
  fills: Fill[];

  // Container Properties
  /**
   * The children shapes contained within the board.
   * When writing into this property, you can only reorder the shapes, not
   * changing the structure. If the new shapes don't match the current shapes
   * it will give a validation error.
   *
   * @example
   * ```js
   * board.children = board.children.reverse();
   * ```
   */
  children: Shape[];

  /**
   * Appends a child shape to the board.
   * @param child The child shape to append.
   *
   * @example
   * ```js
   * board.appendChild(childShape);
   * ```
   */
  appendChild(child: Shape): void;

  /**
   * Inserts a child shape at the specified index within the board.
   * @param index The index at which to insert the child shape.
   * @param child The child shape to insert.
   *
   * @example
   * ```js
   * board.insertChild(0, childShape);
   * ```
   */
  insertChild(index: number, child: Shape): void;

  /**
   * Adds a flex layout configuration to the board (so it's necessary to create a board first of all).
   * @return Returns the flex layout configuration added to the board.
   * @example
   * ```js
   * const board = penpot.createBoard();
   * const flex = board.addFlexLayout();
   *
   * // You can change the flex properties as follows.
   * flex.dir = "column";
   * flex.wrap = "wrap";
   * flex.alignItems = "center";
   * flex.justifyContent = "center";
   * flex.horizontalSizing = "fill";
   * flex.verticalSizing = "fill";
   * ```
   */
  addFlexLayout(): FlexLayout;
  /**
   * Adds a grid layout configuration to the board (so it's necessary to create a board first of all). You can add rows and columns, check addRow/addColumn.
   * @return Returns the grid layout configuration added to the board.
   * @example
   * ```js
   * const board = penpot.createBoard();
   * const grid = board.addGridLayout();
   *
   * // You can change the grid properties as follows.
   * grid.alignItems = "center";
   * grid.justifyItems = "start";
   * grid.rowGap = 10;
   * grid.columnGap = 10;
   * grid.verticalPadding = 5;
   * grid.horizontalPadding = 5;
   * ```
   */
  addGridLayout(): GridLayout;

  /**
   * Creates a new ruler guide attached to the board.
   * @param orientation `horizontal` or `vertical`
   * @param value the position of the guide relative to the board
   */
  addRulerGuide(orientation: RulerGuideOrientation, value: number): RulerGuide;

  /**
   * Removes the `guide` from the board.
   */
  removeRulerGuide(guide: RulerGuide): void;

  /**
   * The keyframe animation of the board (Penpot Motion), or `null` when it
   * has none. Only a board at the top of the page has one: it animates the
   * board and the layers inside it. The component copies in the board play
   * the animation of their mains even without one, see `animatedCopies`.
   *
   * @example
   * ```js
   * const timeline = board.timeline ?? board.addTimeline({ duration: 1500 });
   * ```
   */
  readonly timeline: Timeline | null;

  /**
   * Gives the board a keyframe animation (Penpot Motion), or the one it has
   * the settings of `options`, and returns it. Only a board at the top of
   * the page has one. Needs the Motion feature (`animation/v1`) and the
   * `content:write` permission.
   * @param options The name, the duration in milliseconds and what playback
   * does at the end.
   *
   * @example
   * ```js
   * const timeline = board.addTimeline({ duration: 2000, playback: 'loop' });
   * ```
   */
  addTimeline(options?: TimelineOptions): Timeline;

  /**
   * The component copies in the board that play the animation of their
   * mains: an animated component animates every copy of it, in any board,
   * and a component inside a component plays the animation of its own
   * main. Each plays on its own clock, where the copy is and as big as it
   * is. A layer the timeline of the board animates keeps that animation.
   * The copies play the audio clips of the board of their main too, on
   * their clock: once for all the copies of a main in the board, in the
   * workspace, in view mode and in MP4 and WebM exports (a public share
   * link plays no sound of a library's components).
   *
   * @example
   * ```js
   * for (const { copy, duration, playback } of board.animatedCopies) {
   *   console.log(copy.name, duration, playback);
   * }
   * ```
   */
  readonly animatedCopies: AnimatedCopy[];

  /**
   * @return Returns true when the current board is a VariantContainer
   */
  isVariantContainer(): boolean;
}

/**
 * Represents a VariantContainer in Penpot
 * This interface extends `Board` and includes properties and methods specific to VariantContainer.
 */
export interface VariantContainer extends Board {
  /**
   * Access to the Variant interface, for attributes and actions over the full Variant (not only this VariantContainer)
   */
  readonly variants: Variants | null;
}

/**
 * Represents a boolean operation shape in Penpot.
 * This interface extends `ShapeBase` and includes properties and methods specific to boolean operations.
 */
export interface Boolean extends ShapeBase {
  /**
   * The type of the shape, which is always 'boolean' for boolean operation shapes.
   */
  readonly type: 'boolean';

  /**
   * Converts the boolean shape to its path data representation.
   * @return Returns the path data (d attribute) as a string.
   * @deprecated Use the `d` attribute
   */
  toD(): string;

  /**
   * The content of the boolean shape, defined as the path string.
   * @deprecated Use either `d` or `commands`.
   */
  readonly content: string;

  /**
   * The content of the boolean shape, defined as the path string.
   */
  readonly d: string;

  /**
   * The content of the boolean shape, defined as an array of path commands.
   */
  readonly commands: Array<PathCommand>;

  /**
   * The fills applied to the shape.
   */
  fills: Fill[];

  // Container Properties
  /**
   * The children shapes contained within the boolean shape.
   */
  readonly children: Shape[];
  /**
   * Appends a child shape to the boolean shape.
   * @param child The child shape to append.
   *
   * @example
   * ```js
   * boolShape.appendChild(childShape);
   * ```
   */
  appendChild(child: Shape): void;
  /**
   * Inserts a child shape at the specified index within the boolean shape.
   * @param index The index at which to insert the child shape.
   * @param child The child shape to insert.
   *
   * @example
   * ```js
   * boolShape.insertChild(0, childShape);
   * ```
   */
  insertChild(index: number, child: Shape): void;
}

/**
 * Represents the boolean operation types available in Penpot.
 * These types define how shapes can be combined or modified using boolean operations.
 */
export type BooleanType = 'union' | 'difference' | 'exclude' | 'intersection';

/**
 * Bounds represents the boundaries of a rectangular area,
 * defined by the coordinates of the top-left corner and the dimensions of the rectangle.
 *
 * @example
 * ```js
 * const bounds = { x: 50, y: 50, width: 200, height: 100 };
 * console.log(bounds);
 * ```
 */
export type Bounds = {
  /**
   * Top-left x position of the rectangular area defined
   */
  readonly x: number;
  /**
   * Top-left y position of the rectangular area defined
   */
  readonly y: number;
  /**
   * Width of the represented area
   */
  readonly width: number;
  /**
   * Height of the represented area
   */
  readonly height: number;
};

/**
 * This action will close a targeted board that is opened as an overlay.
 */
export interface CloseOverlay {
  /**
   * The action type
   */
  readonly type: 'close-overlay';

  /**
   * The overlay to be closed with this action.
   */
  readonly destination?: Board;

  /**
   * Animation displayed with this interaction. Omit it to close with no transition.
   */
  readonly animation?: Animation;
}

/**
 * Represents color properties in Penpot.
 * This interface includes properties for defining solid colors, gradients, and image fills, along with metadata.
 */
export interface Color {
  /**
   * The optional reference ID for an external color definition.
   */
  id?: string;
  /**
   * The optional reference to an external file for the color definition.
   */
  fileId?: string;
  /**
   * The optional name of the color.
   */
  name?: string;
  /**
   * The optional path or category to which this color belongs.
   */
  path?: string;
  /**
   * The optional solid color, represented as a string (e.g., '#FF5733').
   */
  color?: string;
  /**
   * The optional opacity level of the color, ranging from 0 (fully transparent) to 1 (fully opaque).
   * Defaults to 1 if omitted.
   */
  opacity?: number;
  /**
   * The optional reference ID for an external color definition.
   * @deprecated Use `id` instead
   */
  refId?: string;
  /**
   * The optional reference to an external file for the color definition.
   * @deprecated Use `fileId`
   */
  refFile?: string;
  /**
   * The optional gradient fill defined by a Gradient object.
   */
  gradient?: Gradient;
  /**
   * The optional image fill defined by an ImageData object.
   */
  image?: ImageData;
}

/**
 * Additional color information for the methods to extract colors from a list of shapes.
 */
export interface ColorShapeInfo {
  /**
   * List of shapes with additional information
   */
  readonly shapesInfo: ColorShapeInfoEntry[];
}

/**
 * Entry for the color shape additional information.
 */
export interface ColorShapeInfoEntry {
  /**
   * Property that has the color (example: fill, stroke...)
   */
  readonly property: string;

  /**
   * For properties that are indexes (such as fill) represent the index
   * of the color inside that property.
   */
  readonly index?: number;

  /**
   * Identifier of the shape that contains the color
   */
  readonly shapeId: string;
}

/**
 * Comments allow the team to have one priceless conversation getting and
 * providing feedback right over the designs and prototypes.
 */
export interface Comment {
  /**
   * The `user` that has created the comment.
   */
  readonly user: User;

  /**
   * The `date` the comment has been created.
   */
  readonly date: Date;

  /**
   * The `content` for the commentary. The owner can modify the comment.
   */
  content: string;

  /**
   * Remove the current comment from its comment thread. Only the owner can remove their comments.
   * Requires the `comment:write` permission.
   */
  remove(): void;
}

/**
 * Represents a list of comments one after the other. Usually these threads
 * are conversations the users have in Penpot.
 */
export interface CommentThread {
  /**
   * This is the number that is displayed on the workspace. Is an increasing
   * sequence for each comment.
   */
  readonly seqNumber: number;

  /**
   * If the thread is attached to a `board` this will have that board
   * reference.
   */
  readonly board?: Board;

  /**
   * Owner of the comment thread
   */
  readonly owner?: User;

  /**
   * The `position` in absolute coordinates in the canvas.
   */
  position: Point;

  /**
   * Whether the thread has been marked as `resolved` or not.
   */
  resolved: boolean;

  /**
   * List of `comments` ordered by creation date.
   * Requires the `comment:read` or `comment:write` permission.
   */
  findComments(): Promise<Comment[]>;

  /**
   * Creates a new comment after the last one in the thread. The current user will
   * be used as the creation user.
   * Requires the `comment:write` permission.
   */
  reply(content: string): Promise<Comment>;

  /**
   * Removes the current comment thread. Only the user that created the thread can
   * remove it.
   * Requires the `comment:write` permission.
   */
  remove(): void;
}

/**
 * CommonLayout represents a common layout interface in the Penpot application.
 * It includes various properties for alignment, spacing, padding, and sizing, as well as a method to remove the layout.
 */
export interface CommonLayout {
  /**
   * The `alignItems` property specifies the default alignment for items inside the container.
   * It can be one of the following values:
   * - 'start': Items are aligned at the start.
   * - 'end': Items are aligned at the end.
   * - 'center': Items are centered.
   * - 'stretch': Items are stretched to fill the container.
   */
  alignItems?: 'start' | 'end' | 'center' | 'stretch';
  /**
   * The `alignContent` property specifies how the content is aligned within the container when there is extra space.
   * It can be one of the following values:
   * - 'start': Content is aligned at the start.
   * - 'end': Content is aligned at the end.
   * - 'center': Content is centered.
   * - 'space-between': Content is distributed with space between.
   * - 'space-around': Content is distributed with space around.
   * - 'space-evenly': Content is distributed with even space around.
   * - 'stretch': Content is stretched to fill the container.
   */
  alignContent?:
    | 'start'
    | 'end'
    | 'center'
    | 'space-between'
    | 'space-around'
    | 'space-evenly'
    | 'stretch';
  /**
   * The `justifyItems` property specifies the default justification for items inside the container.
   * It can be one of the following values:
   * - 'start': Items are justified at the start.
   * - 'end': Items are justified at the end.
   * - 'center': Items are centered.
   * - 'stretch': Items are stretched to fill the container.
   */
  justifyItems?: 'start' | 'end' | 'center' | 'stretch';
  /**
   * The `justifyContent` property specifies how the content is justified within the container when there is extra space.
   * It can be one of the following values:
   * - 'start': Content is justified at the start.
   * - 'center': Content is centered.
   * - 'end': Content is justified at the end.
   * - 'space-between': Content is distributed with space between.
   * - 'space-around': Content is distributed with space around.
   * - 'space-evenly': Content is distributed with even space around.
   * - 'stretch': Content is stretched to fill the container.
   */
  justifyContent?:
    | 'start'
    | 'center'
    | 'end'
    | 'space-between'
    | 'space-around'
    | 'space-evenly'
    | 'stretch';

  /**
   * The `rowGap` property specifies the gap between rows in the layout.
   */
  rowGap: number;
  /**
   * The `columnGap` property specifies the gap between columns in the layout.
   */
  columnGap: number;

  /**
   * The `verticalPadding` property specifies the vertical padding inside the container.
   */
  verticalPadding: number;
  /**
   * The `horizontalPadding` property specifies the horizontal padding inside the container.
   */
  horizontalPadding: number;

  /**
   * The `topPadding` property specifies the padding at the top of the container.
   */
  topPadding: number;
  /**
   * The `rightPadding` property specifies the padding at the right of the container.
   */
  rightPadding: number;
  /**
   * The `bottomPadding` property specifies the padding at the bottom of the container.
   */
  bottomPadding: number;
  /**
   * The `leftPadding` property specifies the padding at the left of the container.
   */
  leftPadding: number;
  /**
   * The `paddingType` property specifies how the four padding values are applied.
   * It can be one of the following values:
   * - 'simple': the vertical and horizontal paddings are mirrored across both sides.
   * - 'multiple': each of the four sides (top, right, bottom, left) is honoured independently.
   */
  paddingType: 'simple' | 'multiple';

  /**
   * The `horizontalSizing` property specifies the horizontal sizing behavior of the container.
   * It can be one of the following values:
   * - 'fix': The container has its own intrinsic fixed size.
   * - 'fill': The container fills the available space. Only can be set if it's inside another layout.
   * - 'auto': The container fits the content.
   */
  horizontalSizing: 'fix' | 'fill' | 'auto';
  /**
   * The `verticalSizing` property specifies the vertical sizing behavior of the container.
   * It can be one of the following values:
   * - 'fix': The container has its own intrinsic fixed size.
   * - 'fill': The container fills the available space. Only can be set if it's inside another layout.
   * - 'auto': The container fits the content.
   */
  verticalSizing: 'fix' | 'fill' | 'auto';

  /**
   * The `remove` method removes the layout.
   */
  remove(): void;
}

/** Current workspace of this browser tab. */
export interface WorkspaceContext {
  readonly status: 'none' | 'loading' | 'ready';
  readonly fileId: string | null;
  readonly fileName: string | null;
  readonly teamId: string | null;
}

/**
 * A team the user belongs to. Requires `manage:teams`.
 *
 * Values are a snapshot taken when the team was listed; changes made through
 * this object update it.
 * Property assignments run asynchronously and update values after success.
 * Backend failures are logged to the console; assignments cannot be awaited.
 */
export interface Team {
  readonly id: string;
  /** Whether this is the user's personal team. */
  readonly isDefault: boolean;
  /**
   * The team name. Assigning renames the team; requires `manage:teams`.
   * Trims the name and requires 1 to 250 characters.
   */
  name: string;
  /**
   * Lists the team's projects. Requires `content:read`.
   *
   * @example
   * ```ts
   * const [team] = await penpotMgmt.listTeams();
   * const projects = await team.listProjects();
   * ```
   */
  listProjects(): Promise<Project[]>;
  /** Lists projects still recoverable from Trash. Requires `content:read`. */
  listDeletedProjects(): Promise<Project[]>;
  /** Lists files still recoverable from Trash. Requires `content:read`. */
  listDeletedFiles(): Promise<ProjectFile[]>;
  /**
   * Creates a project in the team. Requires `manage:projects`.
   *
   * - Trims `name` and requires 1 to 250 characters.
   * - Does not change navigation.
   *
   * @example
   * ```ts
   * const project = await team.createProject({ name: 'Design system' });
   * ```
   */
  createProject(options: { name: string }): Promise<Project>;
}

/**
 * A project in the dashboard.
 *
 * Values are a snapshot taken when the project was listed; changes made through
 * this object update it.
 * Property assignments run asynchronously and update values after success.
 * Backend failures are logged to the console; assignments cannot be awaited.
 */
export interface Project {
  readonly id: string;
  readonly teamId: string;
  /** Whether this is the team's Drafts project. */
  readonly isDefault: boolean;
  /** Number of files when the project was listed; file operations do not update it. */
  readonly fileCount: number;
  /**
   * The project name. Assigning renames the project; requires `manage:projects`.
   * Trims the name and requires 1 to 250 characters.
   */
  name: string;
  /** Whether the project is pinned in the sidebar. Assigning requires `manage:projects`. */
  pinned: boolean;
  /**
   * Lists the files in the project. Requires `content:read`.
   *
   * @example
   * ```ts
   * const files = await project.listFiles();
   * ```
   */
  listFiles(): Promise<ProjectFile[]>;
  /** Lists the project's files still recoverable from Trash. Requires `content:read`. */
  listDeletedFiles(): Promise<ProjectFile[]>;
  /**
   * Restores the project through its files using the dashboard's Trash operation.
   * Requires `manage:projects` and team edit access. Does not require `manage:delete`.
   * Rejects if the project has no recoverable files, including empty projects.
   */
  restore(): Promise<void>;
  /**
   * Creates a file with an initial page in the project. Requires `content:write`.
   *
   * - Trims `name` and requires 1 to 250 characters.
   * - Does not open the file. Use `ProjectFile.open` to edit it.
   *
   * @example
   * ```ts
   * const file = await project.createFile({ name: 'Login' });
   * await file.open();
   * ```
   */
  createFile(options: { name: string }): Promise<ProjectFile>;
  /**
   * Duplicates the project with all its files. Requires `manage:projects`.
   *
   * `name` defaults to the name the backend chooses.
   */
  duplicate(options?: { name?: string }): Promise<Project>;
  /** Moves the project to another team. Requires `manage:teams`. */
  moveTo(team: Team): Promise<void>;
  /**
   * Moves the project and its files to Trash. Requires `manage:delete`.
   * They can be restored during the retention period. Cannot permanently delete.
   */
  remove(): Promise<void>;
}

/**
 * A file in the dashboard, which may or may not be open.
 *
 * Values are a snapshot taken when the file was listed; changes made through
 * this object update it. To read or edit its content, `open` it and use
 * `penpot.currentFile`.
 * Property assignments run asynchronously and update values after success.
 * Backend failures are logged to the console; assignments cannot be awaited.
 */
export interface ProjectFile {
  readonly id: string;
  readonly teamId: string;
  readonly projectId: string;
  /** Last modification time as an ISO 8601 string in UTC. */
  readonly modifiedAt: string;
  /**
   * The file name. Assigning renames the file; requires `content:write`.
   * Trims the name and requires 1 to 250 characters.
   */
  name: string;
  /**
   * Whether the file is shared as a library with its team. Assigning requires
   * `library:write`. A shared file is listed by
   * `penpot.library.availableLibraries()`; unsharing copies its assets into
   * every file that uses it and disconnects them.
   */
  shared: boolean;
  /**
   * Opens the file in this tab and resolves once its workspace and page are
   * ready. Requires `content:read`.
   */
  open(): Promise<void>;
  /**
   * Restores the file using the dashboard's Trash operation, including its parent
   * project if deleted. Requires `content:write` and team edit access.
   * Rejects if the file no longer exists. Does not require `manage:delete`.
   */
  restore(): Promise<void>;
  /**
   * Moves the file to another project of the same team. Requires `content:write`.
   * Rejects a destination that currently belongs to another team.
   */
  moveTo(project: Project): Promise<void>;
  /**
   * Duplicates the file in its project. Requires `content:write`.
   *
   * `name` defaults to the name the backend chooses.
   */
  duplicate(options?: { name?: string }): Promise<ProjectFile>;
  /**
   * Moves the file to Trash. Requires `manage:delete`.
   * It can be restored during the retention period. Cannot permanently delete.
   */
  remove(): Promise<void>;
}

/**
 * Application management API exposed as `penpotMgmt` to global plugins.
 *
 * - Lists teams, projects and files, and opens files, from the dashboard or a
 *   file workspace. Teams, projects and files are objects with their own
 *   properties and methods.
 * - Listing and opening require `content:read`; teams require `manage:teams`.
 * - File operations on `penpot` require a ready workspace.
 * - Reacquire file, page and shape objects after changing files.
 */
export interface PenpotMgmt {
  /** Current workspace availability and metadata; usable for file operations when status is `ready`. */
  readonly workspace: WorkspaceContext;
  /**
   * Lists the teams of the user. Requires `manage:teams`.
   *
   * @example
   * ```ts
   * const teams = await penpotMgmt.listTeams();
   * ```
   */
  listTeams(): Promise<Team[]>;
  /**
   * Creates a team with the user as owner. Requires `manage:teams`.
   *
   * - Trims `name` and requires 1 to 250 characters.
   * - Does not change navigation.
   *
   * @example
   * ```ts
   * const team = await penpotMgmt.createTeam({ name: 'Marketing' });
   * ```
   */
  createTeam(options: { name: string }): Promise<Team>;
  /**
   * Lists the projects of a team. Requires `content:read`.
   *
   * - `teamId` defaults to the current team.
   * - Rejects invalid identifiers and backend access errors.
   *
   * @example
   * ```ts
   * const projects = await penpotMgmt.listProjects();
   * ```
   */
  listProjects(options?: { teamId?: string }): Promise<Project[]>;
  /**
   * Lists projects still recoverable from Trash. Requires `content:read`.
   * `teamId` defaults to the current team. Use `Project.restore` to recover them.
   */
  listDeletedProjects(options?: { teamId?: string }): Promise<Project[]>;
  /**
   * Lists files still recoverable from Trash. Requires `content:read`.
   * `teamId` defaults to the current team. Use `ProjectFile.restore` to recover them.
   */
  listDeletedFiles(options?: { teamId?: string }): Promise<ProjectFile[]>;
  /**
   * Loads a file by id, open or not. Requires `content:read`.
   *
   * @example
   * ```ts
   * const file = await penpotMgmt.getFile(penpotMgmt.workspace.fileId!);
   * file.name = 'Checkout';
   * ```
   */
  getFile(fileId: string): Promise<ProjectFile>;
  /**
   * Opens a file in this tab and resolves once its workspace and page are ready.
   *
   * - `teamId` defaults to the current team.
   * - Rejects invalid identifiers, failed or superseded navigation, and waits exceeding 30 seconds.
   *
   * @example
   * ```ts
   * await penpotMgmt.openFile(file.id, { teamId: file.teamId });
   * ```
   */
  openFile(fileId: string, options?: { teamId?: string }): Promise<void>;
  /** Subscribes to workspace availability and metadata changes. */
  on(
    type: 'workspacechange',
    callback: (workspace: WorkspaceContext) => void,
  ): symbol;
  off(listenerId: symbol): void;
}

/**
 * Represents the context of Penpot, providing access to various Penpot functionalities and data.
 */
export interface Context {
  /**
   * Backs `penpotMgmt`. Its events go through the plugin listener registry,
   * so `on` and `off` are not part of it.
   */
  readonly management?: Omit<PenpotMgmt, 'on' | 'off'>;
  /**
   * Returns the current penpot version.
   */
  readonly version: string;

  /**
   * The root shape in the current Penpot context. Requires `content:read` permission.
   *
   * @example
   * ```js
   * const rootShape = context.root;
   * console.log(rootShape);
   * ```
   */
  readonly root: Shape | null;
  /**
   * Retrieves file data from the current Penpot context. Requires `content:read` permission.
   * @return Returns the file data or `null` if no file is available.
   *
   * @example
   * ```js
   * const fileData = context.currentFile;
   * console.log(fileData);
   * ```
   */
  readonly currentFile: File | null;
  /**
   * The current page in the Penpot context. Requires `content:read` permission.
   *
   * @example
   * ```js
   * const currentPage = context.currentPage;
   * console.log(currentPage);
   * ```
   */
  readonly currentPage: Page | null;
  /**
   * The viewport settings in the Penpot context.
   *
   * @example
   * ```js
   * const viewportSettings = context.viewport;
   * console.log(viewportSettings);
   * ```
   */
  readonly viewport: Viewport;

  /**
   * Provides flags to customize the API behavior.
   */
  readonly flags: Flags;

  /**
   * Context encapsulating the history operations
   *
   * @example
   * ```js
   * const historyContext = context.history;
   * console.log(historyContext);
   * ```
   */
  readonly history: HistoryContext;

  /**
   * The library context in the Penpot context, including both local and connected libraries. Requires `library:read` permission.
   *
   * @example
   * ```js
   * const libraryContext = context.library;
   * console.log(libraryContext);
   * ```
   */
  readonly library: LibraryContext;
  /**
   * The fonts context in the Penpot context, providing methods to manage fonts. Requires `content:read` permission.
   *
   * @example
   * ```js
   * const fontsContext = context.fonts;
   * console.log(fontsContext);
   * ```
   */
  readonly fonts: FontsContext;
  /**
   * The current user in the Penpot context. Requires `user:read` permission.
   *
   * @example
   * ```js
   * const currentUser = context.currentUser;
   * console.log(currentUser);
   * ```
   */
  readonly currentUser: User;
  /**
   * An array of active users in the Penpot context. Requires `user:read` permission.
   *
   * @example
   * ```js
   * const activeUsers = context.activeUsers;
   * console.log(activeUsers);
   * ```
   */
  readonly activeUsers: ActiveUser[];

  /**
   * The current theme (light or dark) in Penpot.
   *
   * @example
   * ```js
   * const currentTheme = context.theme;
   * console.log(currentTheme);
   * ```
   */
  readonly theme: Theme;

  /**
   * Access to the localStorage proxy
   */
  readonly localStorage: LocalStorage;

  /**
   * The currently selected shapes in Penpot. Requires `content:read` permission.
   *
   * @example
   * ```js
   * const selectedShapes = context.selection;
   * console.log(selectedShapes);
   * ```
   */
  selection: Shape[];

  /**
   * Retrieves colors applied to the given shapes in Penpot. Requires `content:read` permission.
   * @return Returns an array of colors and their shape information.
   *
   * @example
   * ```js
   * const colors = context.shapesColors(shapes);
   * console.log(colors);
   * ```
   */
  shapesColors(shapes: Shape[]): (Color & ColorShapeInfo)[];

  /**
   * Replaces a specified old color with a new color in the given shapes. Requires `content:write` permission.
   *
   * @example
   * ```js
   * context.replaceColor(shapes, oldColor, newColor);
   * ```
   */
  replaceColor(shapes: Shape[], oldColor: Color, newColor: Color): void;

  /**
   * Uploads media to Penpot and retrieves its image data. Requires `content:write` permission.
   * @param name The name of the media.
   * @param url The URL of the media to be uploaded.
   * @return Returns a promise that resolves to the image data of the uploaded media.
   *
   * @example
   * ```js
   * const imageData = await context.uploadMediaUrl('example', 'https://example.com/image.jpg');
   * console.log(imageData);
   *
   * // to insert the image in a shape we can do
   * const board = penpot.createBoard();
   * const shape = penpot.createRectangle();
   * board.appendChild(shape);
   * shape.fills = [{ fillOpacity: 1, fillImage: imageData }];
   * ```
   */
  uploadMediaUrl(name: string, url: string): Promise<ImageData>;

  /**
   * Uploads media to penpot and retrieves the image data. Requires `content:write` permission.
   * @param name The name of the media.
   * @param data The image content data
   * @return Returns a promise that resolves to the image data of the uploaded media.
   *
   * @example
   * ```js
   * const imageData = await context.uploadMediaData('example', imageData, 'image/jpeg');
   * console.log(imageData);
   * ```
   */
  uploadMediaData(
    name: string,
    data: Uint8Array,
    mimeType: string,
  ): Promise<ImageData>;

  /**
   * Groups the specified shapes. Requires `content:write` permission.
   * @param shapes - An array of shapes to group.
   * @return Returns the newly created group or `null` if the group could not be created.
   * @example
   * ```js
   * const penpotShapesArray = penpot.selection;
   * penpot.group(penpotShapesArray);
   * ```
   */
  group(shapes: Shape[]): Group | null;
  /**
   * Ungroups the specified group. Requires `content:write` permission.
   * @param group - The group to ungroup.
   * @param other - Additional groups to ungroup.
   *
   * @example
   * ```js
   * const penpotShapesArray = penpot.selection;
   * // We need to make sure that something is selected, and that the selected shape is a group
   * if (penpotShapesArray.length && penpot.utils.types.isGroup(penpotShapesArray[0])) {
   *   penpot.ungroup(penpotShapesArray[0]);
   * }
   * ```
   */
  ungroup(group: Group, ...other: Group[]): void;

  /**
   * Use this method to create the shape of a rectangle. Requires `content:write` permission.
   *
   * @example
   * ```js
   * const shape = penpot.createRectangle();
   * // just change the values like this
   * shape.name = "Example rectangle";
   *
   * // for solid color
   * shape.fills = [{ fillColor: "#7EFFF5" }];
   * // for linear gradient color
   * shape.fills = [{
   *  fillColorGradient: {
   *    "type": "linear",
   *    "startX": 0.5,
   *    "startY": 0,
   *    "endX": 0.5,
   *    "endY": 1,
   *    "width": 1,
   *    "stops": [
   *      {
   *        "color": "#003ae9",
   *        "opacity": 1,
   *        "offset": 0
   *      },
   *      {
   *        "color": "#003ae9",
   *        "opacity": 0,
   *        "offset": 1
   *      }
   *    ]
   *  }
   *}];
   * // for a image fill
   * const imageData = await context.uploadMediaUrl('example', 'https://example.com/image.jpg');
   * shape.fills = [{ fillOpacity: 1, fillImage: imageData }];
   *
   * shape.borderRadius = 8;
   * shape.strokes = [
   *  {
   *    strokeColor: "#2e3434",
   *    strokeStyle: "solid",
   *    strokeWidth: 2,
   *    strokeAlignment: "center",
   *  },
   *];
   * ```
   */
  createRectangle(): Rectangle;
  /**
   * Use this method to create a board. This is the first step before anything else, the container. Requires `content:write` permission.
   * Then you can add a gridlayout, flexlayout or add a shape inside the board.
   * Just a heads-up: board is a board in Penpot UI.
   *
   * @example
   * ```js
   * const board = penpot.createBoard();
   *
   * // to add grid layout
   * board.addGridLayout();
   * // to add flex layout
   * board.addFlexLayout();
   *
   * // to create a shape inside the board
   * const shape = penpot.createRectangle();
   * board.appendChild(shape);
   * ```
   */
  createBoard(): Board;
  /**
   * Use this method to create the shape of an ellipse. Requires `content:write` permission.
   *
   * @example
   * ```js
   * const shape = penpot.createEllipse();
   * // just change the values like this
   * shape.name = "Example ellipse";
   *
   * // for solid color
   * shape.fills = [{ fillColor: "#7EFFF5" }];
   * // for linear gradient color
   * shape.fills = [{
   *  fillColorGradient: {
   *    "type": "linear",
   *    "startX": 0.5,
   *    "startY": 0,
   *    "endX": 0.5,
   *    "endY": 1,
   *    "width": 1,
   *    "stops": [
   *      {
   *        "color": "#003ae9",
   *        "opacity": 1,
   *        "offset": 0
   *      },
   *      {
   *        "color": "#003ae9",
   *        "opacity": 0,
   *        "offset": 1
   *      }
   *    ]
   *  }
   *}];
   * // for an image fill
   * const imageData = await context.uploadMediaUrl('example', 'https://example.com/image.jpg');
   * shape.fills = [{ fillOpacity: 1, fillImage: imageData }];
   *
   * shape.strokes = [
   *  {
   *    strokeColor: "#2e3434",
   *    strokeStyle: "solid",
   *    strokeWidth: 2,
   *    strokeAlignment: "center",
   *  },
   *];
   * ```
   */
  createEllipse(): Ellipse;
  /**
   * Use this method to create a path. Requires `content:write` permission.
   *
   * @example
   * ```js
   * const path = penpot.createPath();
   * path.name = "My path";
   *
   * // for solid color
   * path.fills = [{ fillColor: "#7EFFF5" }];
   *
   * ```
   */
  createPath(): Path;
  /**
   * Creates a Boolean shape based on the specified boolean operation and shapes. Requires `content:write` permission.
   * @param boolType The type of boolean operation ('union', 'difference', 'exclude', 'intersection').
   * @param shapes An array of shapes to perform the boolean operation on.
   * @return Returns the newly created Boolean shape resulting from the boolean operation.
   *
   * @example
   * ```js
   * const booleanShape = context.createBoolean('union', [shape1, shape2]);
   * ```
   */
  createBoolean(boolType: BooleanType, shapes: Shape[]): Boolean | null;
  /**
   * Creates a Group from an SVG string. Requires `content:write` permission.
   * @param svgString The SVG string representing the shapes to be converted into a group.
   * @return Returns the newly created Group containing the shapes from the SVG.
   *
   * @example
   * ```js
   * const svgGroup = context.createShapeFromSvg('<svg>...</svg>');
   * ```
   */
  createShapeFromSvg(svgString: string): Group | null;
  /**
   * Creates a Group from an SVG string. The SVG can have images and the method returns
   * a Promise because the shape will be available after all images are uploaded.
   * Requires `content:write` permission.
   * @param svgString The SVG string representing the shapes to be converted into a group.
   * @return Returns a promise with the newly created Group containing the shapes from the SVG.
   *
   * @example
   * ```js
   * const svgGroup = await context.createShapeFromSvgWithImages('<svg>...</svg>');
   * ```
   */
  createShapeFromSvgWithImages(svgString: string): Promise<Group | null>;

  /**
   * Creates a Text shape with the specified text content. Requires `content:write` permission.
   * @param text The text content for the Text shape. Must be a non-empty string.
   * @return Returns the new created shape. Returns null if an empty string is provided or the shape couldn't be created.
   *
   * @example
   * ```js
   * const board = penpot.createBoard();
   * let text;
   * text = penpot.createText();
   * // just change the values like this
   * text.growType = 'auto-height';
   * text.fontFamily = 'Work Sans';
   * text.fontSize = '12';
   * text.fills = [{fillColor: '#9f05ff', fillOpacity: 1}];
   * text.strokes = [{strokeOpacity: 1, strokeStyle: 'solid', strokeWidth: 2, strokeColor: '#deabff', strokeAlignment: 'outer'}];
   * board.appendChild(text);
   * ```
   */
  createText(text: string): Text | null;

  /**
   * Generates markup for the given shapes. Requires `content:read` permission
   * @param shapes the shapes to generate the markup for
   * @param options `type` of the markup to generate. Defaults to `'html'`.
   *
   * @example
   * ```js
   * const markup = context.generateMarkup(shapes, { type: 'html' });
   * console.log(markup);
   * ```
   */
  generateMarkup(shapes: Shape[], options?: { type?: 'html' | 'svg' }): string;

  /**
   * Generates styles for the given shapes. Requires `content:read` permission
   * @param shapes the shapes to generate the styles for
   * @param options `type` of the styles to generate (defaults to `'css'`), `withPrelude` to include the style prelude
   * (defaults to `false`) and `includeChildren` to also generate the styles for the shape children (defaults to `true`).
   *
   * @example
   * ```js
   * const styles = context.generateStyle(shapes, { type: 'css' });
   * console.log(styles);
   * ```
   */
  generateStyle(
    shapes: Shape[],
    options?: {
      type?: 'css';
      withPrelude?: boolean;
      includeChildren?: boolean;
    },
  ): string;

  /**
   * Generates the fontfaces styles necessaries to render the shapes.
   * Requires `content:read` permission
   * @param shapes
   *
   * @example
   * ```js
   * const fontfaces = context.generateFontFaces(penpot.selection);
   * console.log(fontfaces);
   * ```
   */
  generateFontFaces(shapes: Shape[]): Promise<string>;

  /**
   * Adds the current callback as an event listener
   *
   * @example
   * ```js
   * const listenerId = context.addListener('selectionchange', (event) => {
   *   console.log(event);
   * });
   * ```
   */
  addListener<T extends keyof EventsMap>(
    type: T,
    callback: (event: EventsMap[T]) => void,
    props?: { [key: string]: unknown },
  ): symbol;

  /**
   * Removes the listenerId from the list of listeners
   *
   * @example
   * ```js
   * context.removeListener(listenerId);
   * ```
   */
  removeListener(listenerId: symbol): void;

  /**
   * Opens the viewer section. Requires `content:read` permission.
   */
  openViewer(): void;

  /**
   * Creates a new page and returns it. Requires `content:write` permission.
   *
   * IMPORTANT: creating a page does **not** make it the active page.
   * To build content inside the new page, activate it first with
   * {@link Context.openPage} (and `await` it) before mutate shapes:
   *
   * @example
   * ```js
   * const page = penpot.createPage();
   * page.name = 'New Page';
   * await penpot.openPage(page); // make the new page active first
   * const board = penpot.createBoard();
   * board.resize(375, 812);
   * page.root.appendChild(board);
   * ```
   */
  createPage(): Page;

  /**
   * Changes the current open page to the given page, making it the **active page**.
   * The active page is the one all shape creation and structural operations
   * (`createBoard`, `appendChild`, `insertChild`, property setters, etc.) act upon,
   * so call this (and `await` it) after {@link Context.createPage} before adding
   * shapes to the newly created page. Requires `content:read` permission.
   * @param page the page to open (a Page object or a page UUID string)
   * @param newWindow if true opens the page in a new window, defaults to false
   *
   * @example
   * ```js
   * await context.openPage(page);
   * ```
   */
  openPage(page: Page | string, newWindow?: boolean): Promise<void>;

  /**
   * Aligning will move all the selected layers to a position relative to one
   * of them in the horizontal direction.
   * @param shapes to align
   * @param direction where the shapes will be aligned
   */
  alignHorizontal(
    shapes: Shape[],
    direction: 'left' | 'center' | 'right',
  ): void;

  /**
   * Aligning will move all the selected layers to a position relative to one
   * of them in the vertical direction.
   * @param shapes to align
   * @param direction where the shapes will be aligned
   */
  alignVertical(shapes: Shape[], direction: 'top' | 'center' | 'bottom'): void;

  /**
   * Distributing objects to position them  horizontally with equal distances between them.
   * @param shapes to distribute
   */
  distributeHorizontal(shapes: Shape[]): void;

  /**
   * Distributing objects to position them vertically with equal distances between them.
   * @param shapes to distribute
   */
  distributeVertical(shapes: Shape[]): void;

  /**
   * Converts the shapes into Paths. If the shapes are complex will put together
   * all its paths into one.
   * @param shapes to flatten
   */
  flatten(shapes: Shape[]): Path[];

  /**
   * Combine several standard Components into a VariantComponent. Similar to doing it
   * with the contextual menu on the Penpot interface.
   * All the shapes passed as arguments should be main instances.
   * @param shapes A list of main instances of the components to combine.
   * @return The variant container created
   */
  createVariantFromComponents(shapes: Board[]): VariantContainer;

  /**
   * This method returns a promise that will be resolved when all the
   * pending layout updates have finished and the components have synchronized.
   * If no layout work is pending the promise resolves immediately.
   * @param timeout Maximum time to wait, in milliseconds. If the timeout
   * elapses before the layout settles, the promise is rejected. Defaults to
   * 30000; the promise never waits indefinitely.
   * @return The promise to be resolved when the layout is updated. It is
   * rejected with an Error, both on timeout and on an invalid timeout value.
   */
  waitForLayoutUpdate(timeout?: number): Promise<void>;
}

/**
 * Utility methods for geometric calculations in Penpot.
 *
 * @example
 * ```js
 * const centerPoint = geometryUtils.center(shapes);
 * console.log(centerPoint);
 * ```
 */
export interface ContextGeometryUtils {
  /**
   * Calculates the center point of a given array of shapes.
   * This method computes the geometric center (centroid) of the bounding boxes of the provided shapes.
   * @param shapes - The array of shapes to calculate the center for.
   * @return Returns the center point as an object with `x` and `y` coordinates, or null if the array is empty.
   *
   * @example
   * ```js
   * const centerPoint = geometryUtils.center(shapes);
   * console.log(centerPoint);
   * ```
   */
  center(shapes: Shape[]): { x: number; y: number } | null;
}

/**
 * Utility methods for determining the types of Penpot shapes.
 *
 * @example
 * ```js
 * const isBoard = typesUtils.isBoard(shape);
 * console.log(isBoard);
 * ```
 */
export interface ContextTypesUtils {
  /**
   * Checks if the given shape is a board.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a board, otherwise false.
   */
  isBoard(shape: Shape): shape is Board;

  /**
   * Checks if the given shape is a group.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a Group, otherwise false.
   */
  isGroup(shape: Shape): shape is Group;

  /**
   * Checks if the given shape is a mask.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a Group (acting as a mask), otherwise false.
   */
  isMask(shape: Shape): shape is Group;

  /**
   * Checks if the given shape is a boolean operation.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a Bool, otherwise false.
   */
  isBool(shape: Shape): shape is Boolean;

  /**
   * Checks if the given shape is a rectangle.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a Rectangle, otherwise false.
   */
  isRectangle(shape: Shape): shape is Rectangle;

  /**
   * Checks if the given shape is a path.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a Path, otherwise false.
   */
  isPath(shape: Shape): shape is Path;

  /**
   * Checks if the given shape is a text element.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a Text, otherwise false.
   */
  isText(shape: Shape): shape is Text;

  /**
   * Checks if the given shape is an ellipse.
   * @param shape - The shape to check.
   * @return Returns true if the shape is an Ellipse, otherwise false.
   */
  isEllipse(shape: Shape): shape is Ellipse;

  /**
   * Checks if the given shape is an SVG.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a SvgRaw, otherwise false.
   */
  isSVG(shape: Shape): shape is SvgRaw;

  /**
   * Checks if the given shape is a variant container.
   * @param shape - The shape to check.
   * @return Returns true if the shape is a variant container, otherwise false.
   */
  isVariantContainer(shape: Shape): shape is VariantContainer;

  /**
   * Checks if the given component is a VariantComponent.
   * @param component - The component to check.
   * @return Returns true if the component is a VariantComponent, otherwise false.
   */
  isVariantComponent(
    component: LibraryComponent,
  ): component is LibraryVariantComponent;
}

/**
 * Utility methods for various operations in Penpot.
 */
export interface ContextUtils {
  /**
   * Geometry utility methods for Penpot.
   * Provides methods for geometric calculations, such as finding the center of a group of shapes.
   *
   * @example
   * ```js
   * const centerPoint = penpot.utils.geometry.center(shapes);
   * console.log(centerPoint);
   * ```
   */
  readonly geometry: ContextGeometryUtils;
  /**
   * Type utility methods for Penpot.
   * Provides methods for determining the types of various shapes in Penpot.
   *
   * @example
   * ```js
   * const isBoard = utils.types.isBoard(shape);
   * console.log(isBoard);
   * ```
   */
  readonly types: ContextTypesUtils;
}

/**
 * Cube animation (Penpot Motion): the board turns away as a side of a cube
 * and the next one comes in on the side next to it.
 * For `navigate-to` only.
 */
export interface Cube {
  /**
   * Type of the animation
   */
  readonly type: 'cube';

  /**
   * The way the near side of the board goes, as with `push`
   */
  readonly direction: 'right' | 'left' | 'up' | 'down';

  /**
   * Duration of the animation effect
   */
  readonly duration: number;

  /**
   * Function that the turn will follow for the interpolation.
   * Defaults to `linear`
   */
  readonly easing?: 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out';
}

/**
 * Dissolve animation
 */
export interface Dissolve {
  /**
   * Type of the animation
   */
  readonly type: 'dissolve';

  /**
   * Duration of the animation effect
   */
  readonly duration: number;

  /**
   * Function that the dissolve effect will follow for the interpolation.
   * Defaults to `linear`.
   */
  readonly easing?: 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out';
}

/**
 * Represents an ellipse shape in Penpot.
 * This interface extends `ShapeBase` and includes properties specific to ellipses.
 */
export interface Ellipse extends ShapeBase {
  /**
   * The type of the shape, which is always 'ellipse' for ellipse shapes.
   */
  readonly type: 'ellipse';

  /**
   * The fills applied to the shape.
   */
  fills: Fill[];
}

/**
 * Represents a mapping of events to their corresponding types in Penpot.
 * This interface provides information about various events that can be triggered in the application.
 *
 * @example
 * ```js
 * penpot.on('pagechange', (event) => {
 *   console.log(event);
 * });
 * ```
 */
export interface EventsMap {
  readonly workspacechange: WorkspaceContext;
  readonly logout: void;
  /**
   * The `pagechange` event is triggered when the active page in the project is changed.
   */
  readonly pagechange: Page;
  /**
   * The `filechange` event is triggered when a different file is opened.
   * The callback will receive the new file.
   */
  readonly filechange: File;
  /**
   * The `selectionchange` event is triggered when the selection of elements changes.
   * This event passes a list of identifiers of the selected elements.
   */
  readonly selectionchange: string[];
  /**
   * The `themechange` event is triggered when the application theme is changed.
   */
  readonly themechange: Theme;
  /**
   * The `finish` event is triggered when the current file is closed.
   * The callback will receive the id of the closed file.
   */
  readonly finish: string;

  /**
   * This event will trigger whenever the shape in the props change. It's mandatory to send
   * with the props an object like `{ shapeId: '<id>' }`
   */
  readonly shapechange: Shape;

  /**
   * The `contentsave` event will trigger after the file content is saved in the backend.
   */
  readonly contentsave: void;
}

/**
 * Represents export settings in Penpot.
 * This interface includes properties for defining export configurations.
 */
export interface Export {
  /**
   * Type of the file to export. Can be one of the following values: png, jpeg, webp, svg, pdf
   */
  type: 'png' | 'jpeg' | 'webp' | 'svg' | 'pdf';
  /**
   * For bitmap formats represent the scale of the original size to resize the export
   */
  scale?: number;
  /**
   * Suffix that will be appended to the resulting exported file
   */
  suffix?: string;
  /**
   * If true will ignore the children when exporting the shape
   */
  skipChildren?: boolean;
}

/**
 * File represents a file in the Penpot application.
 * It includes properties for the file's identifier, name, and revision number.
 */
export interface File extends PluginData {
  /**
   * The `id` property is a unique identifier for the file.
   */
  readonly id: string;

  /**
   * The `name` for the file
   */
  readonly name: string;

  /**
   * The `revn` will change for every document update
   */
  readonly revn: number;

  /**
   * List all the pages for the current file
   */
  readonly pages: Page[];

  /**
   * Export the current file to an archive.
   * @param `exportType` indicates the type of file to generate.
   * - `'penpot'` will create a *.penpot file with a binary representation of the file
   * - `'zip'` will create a *.zip with the file exported in several SVG files with some JSON metadata
   * @param `libraryExportType` indicates what to do with the linked libraries of the file when
   * exporting it. Defaults to `'include-libraries'` if not sent.
   * - `'include-libraries'` will include the libraries as external files that will be exported in a single bundle
   * - `'merge-libraries'` will add all the assets into the main file and only one file will be imported
   * - `'detach-libraries'` will unlink all the external assets and no libraries will be imported
   * - `'link-later'` will preserve component metadata so instances can be relinked on import
   *
   * @example
   * ```js
   * const exportedData = await file.export('penpot', 'include-libraries');
   * ```
   */
  export(
    exportType: 'penpot' | 'zip',
    libraryExportType?:
      | 'include-libraries'
      | 'merge-libraries'
      | 'detach-libraries'
      | 'link-later',
  ): Promise<Uint8Array>;

  /**
   * Retrieves the versions for the file.
   * @param `criteria.createdBy` retrieves only the versions created by the user `createdBy`.
   * Requires the `content:read` permission.
   */
  findVersions(criteria?: { createdBy: User }): Promise<FileVersion[]>;

  /**
   * Saves the current version into the versions history.
   * Requires the `content:write` permission.
   */
  saveVersion(label: string): Promise<FileVersion>;

  /**
   * Runs the referential-integrity validation on the file and returns the list
   * of errors found. An empty array means the file is valid. Useful to detect
   * inconsistencies (dangling references, broken components/variants, …) that
   * the backend would otherwise reject when the file is saved.
   *
   * @example
   * ```js
   * const errors = file.validate();
   * if (errors.length) console.error(errors);
   * ```
   */
  validate(): FileValidationError[];
}

/**
 * A single referential-integrity error reported by {@link File.validate}.
 */
export interface FileValidationError {
  /**
   * The validation error code (e.g. `'variant-component-bad-name'`,
   * `'child-not-found'`).
   */
  readonly code: string;

  /**
   * A human-readable description of the error.
   */
  readonly hint: string;

  /**
   * The id of the offending shape, when the error is attached to one.
   */
  readonly shapeId: string | null;

  /**
   * The id of the page the offending shape lives in, when applicable.
   */
  readonly pageId: string | null;
}

/**
 * Type defining the file version properties.
 */
export interface FileVersion {
  /**
   * The current label to identify the version.
   */
  label: string;

  /**
   * The user that created the version. If not present, the
   * version is an autosave.
   */
  readonly createdBy?: User;

  /**
   * The date when the version was created.
   */
  readonly createdAt: Date;

  /**
   * If the current version has been generated automatically.
   */
  readonly isAutosave: boolean;

  /**
   * Restores the current version and replaces the content of the active file
   * for the contents of this version.
   * Requires the `content:write` permission.
   * Warning: Calling this will close the plugin because the workspace will reload
   */
  restore(): void;

  /**
   * Remove the current version.
   * Requires the `content:write` permission.
   */
  remove(): Promise<void>;

  /**
   * Converts an autosave version into a permanent version.
   * Requires the `content:write` permission.
   */
  pin(): Promise<FileVersion>;
}

/**
 * Represents fill properties in Penpot. You can add a fill to any shape except for groups.
 * This interface includes properties for defining solid color fills, gradient fills, and image fills.
 */
export interface Fill {
  /**
   * The optional solid fill color, represented as a string (e.g., '#FF5733').
   */
  fillColor?: string;
  /**
   * The optional opacity level of the solid fill color, ranging from 0 (fully transparent) to 1 (fully opaque).
   * Defaults to 1 if omitted.
   */
  fillOpacity?: number;
  /**
   * The optional gradient fill defined by a Gradient object.
   */
  fillColorGradient?: Gradient;
  /**
   * The optional reference to an external file for the fill color.
   */
  fillColorRefFile?: string;
  /**
   * The optional reference ID within the external file for the fill color.
   */
  fillColorRefId?: string;
  /**
   * The optional image fill defined by an ImageData object.
   */
  fillImage?: ImageData;
}

/**
 * This subcontext allows the API to change certain defaults
 */
export interface Flags {
  /**
   * If `true` the .children property will always be sorted in the z-index ordering.
   * Also, the appendChild method will append the children in the top-most position.
   * The insertChild method is changed accordingly to respect this ordering.
   * Defaults to false
   */
  naturalChildOrdering: boolean;

  /**
   * If `true` the validation errors will throw an exception instead of displaying an
   * error in the debugger console.
   * Defaults to `true` for plugins whose manifest declares `"version": 2` (or higher)
   * and to `false` for v1 plugins (or manifests that omit the version field).
   */
  throwValidationErrors: boolean;
}

/**
 * Represents a flexible layout configuration in Penpot.
 * This interface extends `CommonLayout` and includes properties for defining the direction,
 * wrapping behavior, and child management of a flex layout.
 */
export interface FlexLayout extends CommonLayout {
  /**
   * The direction of the flex layout.
   * - 'row': Main axis is horizontal, from left to right.
   * - 'row-reverse': Main axis is horizontal, from right to left.
   * - 'column': Main axis is vertical, from top to bottom.
   * - 'column-reverse': Main axis is vertical, from bottom to top.
   */
  dir: 'row' | 'row-reverse' | 'column' | 'column-reverse';
  /**
   * The optional wrapping behavior of the flex layout.
   * - 'wrap': Child elements will wrap onto multiple lines.
   * - 'nowrap': Child elements will not wrap.
   */
  wrap?: 'wrap' | 'nowrap';
  /**
   * Appends a child element to the flex layout.
   * @param child The child element to be appended, of type `Shape`.
   *
   * @example
   * ```js
   * flexLayout.appendChild(childShape);
   * ```
   */
  appendChild(child: Shape): void;
}

/**
 * Flip animation (Penpot Motion): the board turns over like a card, the
 * next one on its back.
 * For `navigate-to` only.
 */
export interface Flip {
  /**
   * Type of the animation
   */
  readonly type: 'flip';

  /**
   * The way the near side of the board goes, as with `push`
   */
  readonly direction: 'right' | 'left' | 'up' | 'down';

  /**
   * Duration of the animation effect
   */
  readonly duration: number;

  /**
   * Function that the turn will follow for the interpolation.
   * Defaults to `linear`
   */
  readonly easing?: 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out';
}

/**
 * Defines an interaction flow inside penpot. A flow is defined by a starting board for an interaction.
 */
export interface Flow {
  /**
   * The page in which the flow is defined
   */
  readonly page: Page;

  /**
   * The name for the current flow
   */
  name: string;

  /**
   * The starting board for this interaction flow
   */
  startingBoard: Board;

  /**
   * Removes the flow from the page
   */
  remove(): void;
}

/**
 * Represents a font in Penpot, which includes details about the font family, variants, and styling options.
 * This interface provides properties and methods for describing and applying fonts within Penpot.
 */
export interface Font {
  /**
   * This property holds the human-readable name of the font.
   */
  readonly name: string;

  /**
   * The unique identifier of the font.
   */
  readonly fontId: string;

  /**
   * The font family of the font.
   */
  readonly fontFamily: string;

  /**
   * The default font style of the font.
   */
  readonly fontStyle?: 'normal' | 'italic' | null;

  /**
   * The default font variant ID of the font.
   */
  readonly fontVariantId: string;

  /**
   * The default font weight of the font.
   */
  readonly fontWeight: string;

  /**
   * An array of font variants available for the font.
   */
  readonly variants: FontVariant[];

  /**
   * Applies the font styles to a text shape.
   * @param text - The text shape to apply the font styles to.
   * @param variant - Optional. The specific font variant to apply. If not provided, applies the default variant.
   *
   * @example
   * ```js
   * font.applyToText(textShape, fontVariant);
   * ```
   */
  applyToText(text: Text, variant?: FontVariant): void;

  /**
   * Applies the font styles to a text range within a text shape.
   * @param range - The text range to apply the font styles to.
   * @param variant - Optional. The specific font variant to apply. If not provided, applies the default variant.
   *
   * @example
   * ```js
   * font.applyToRange(textRange, fontVariant);
   * ```
   */
  applyToRange(range: TextRange, variant?: FontVariant): void;
}

/**
 * Represents a font variant in Penpot, which defines a specific style variation of a font.
 * This interface provides properties for describing the characteristics of a font variant.
 */
export interface FontVariant {
  /**
   * The name of the font variant.
   */
  readonly name: string;

  /**
   * The unique identifier of the font variant.
   */
  readonly fontVariantId: string;

  /**
   * The font weight of the font variant.
   */
  readonly fontWeight: string;

  /**
   * The font style of the font variant.
   */
  readonly fontStyle: 'normal' | 'italic';
}

/**
 * Represents the context for managing fonts in Penpot.
 * This interface provides methods to interact with fonts, such as retrieving fonts by ID or name.
 */
export interface FontsContext {
  /**
   * An array containing all available fonts.
   */
  readonly all: Font[];

  /**
   * Finds a font by its unique identifier.
   * @param id - The ID of the font to find.
   * @return Returns the `Font` object if found, otherwise `null`.
   *
   * @example
   * ```js
   * const font = fontsContext.findById('font-id');
   * if (font) {
   *   console.log(font.name);
   * }
   * ```
   */
  findById(id: string): Font | null;

  /**
   * Finds a font by its name.
   * @param name - The name of the font to find.
   * @return Returns the `Font` object if found, otherwise `null`.
   *
   * @example
   * ```js
   * const font = fontsContext.findByName('font-name');
   * if (font) {
   *   console.log(font.name);
   * }
   * ```
   */
  findByName(name: string): Font | null;

  /**
   * Finds all fonts matching a specific ID.
   * @param id - The ID to match against.
   * @return Returns an array of `Font` objects matching the provided ID.
   *
   * @example
   * ```js
   * const fonts = fontsContext.findAllById('font-id');
   * console.log(fonts);
   * ```
   */
  findAllById(id: string): Font[];

  /**
   * Finds all fonts matching a specific name.
   * @param name - The name to match against.
   * @return Returns an array of `Font` objects matching the provided name.
   *
   * @example
   * ```js
   * const fonts = fontsContext.findAllByName('font-name');
   * console.log(fonts);
   * ```
   */
  findAllByName(name: string): Font[];
}

/**
 * Represents a gradient configuration in Penpot.
 * A gradient can be either linear or radial and includes properties to define its shape, position, and color stops.
 */
export type Gradient = {
  /**
   * Specifies the type of gradient.
   * - 'linear': A gradient that transitions colors along a straight line.
   * - 'radial': A gradient that transitions colors radiating outward from a central point.
   *
   * @example
   * ```js
   * const gradient: Gradient = { type: 'linear', startX: 0, startY: 0, endX: 100, endY: 100, width: 100, stops: [{ color: '#FF5733', offset: 0 }] };
   * ```
   */
  type: 'linear' | 'radial';
  /**
   * The X-coordinate of the starting point of the gradient.
   */
  startX: number;
  /**
   * The Y-coordinate of the starting point of the gradient.
   */
  startY: number;
  /**
   * The X-coordinate of the ending point of the gradient.
   */
  endX: number;
  /**
   * The Y-coordinate of the ending point of the gradient.
   */
  endY: number;
  /**
   * The width of the gradient. For radial gradients, this could be interpreted as the radius.
   */
  width: number;
  /**
   * An array of color stops that define the gradient.
   */
  stops: Array<{ color: string; opacity?: number; offset: number }>;
};

/**
 * GridLayout represents a grid layout in the Penpot application, extending the common layout interface.
 * It includes properties and methods to manage rows, columns, and child elements within the grid.
 */
export interface GridLayout extends CommonLayout {
  /**
   * The `dir` property specifies the primary direction of the grid layout.
   * It can be either 'column' or 'row'.
   */
  dir: 'column' | 'row';
  /**
   * The `rows` property represents the collection of rows in the grid.
   * This property is read-only.
   */
  readonly rows: Track[];
  /**
   * The `columns` property represents the collection of columns in the grid.
   * This property is read-only.
   */
  readonly columns: Track[];

  /**
   * Adds a new row to the grid.
   * @param type The type of the row to add.
   * @param value The value associated with the row type (optional).
   *
   * @example
   * ```js
   * const board = penpot.createBoard();
   * const grid = board.addGridLayout();
   * grid.addRow("flex", 1);
   * ```
   */
  addRow(type: TrackType, value?: number): void;
  /**
   * Adds a new row to the grid at the specified index.
   * @param index The index at which to add the row.
   * @param type The type of the row to add.
   * @param value The value associated with the row type (optional).
   *
   * @example
   * ```js
   * gridLayout.addRowAtIndex(0, 'fixed', 100);
   * ```
   */
  addRowAtIndex(index: number, type: TrackType, value?: number): void;
  /**
   * Adds a new column to the grid.
   * @param type The type of the column to add.
   * @param value The value associated with the column type (optional).
   *
   * @example
   * ```js
   * const board = penpot.createBoard();
   * const grid = board.addGridLayout();
   * grid.addColumn('percent', 50);
   * ```
   */
  addColumn(type: TrackType, value?: number): void;
  /**
   * Adds a new column to the grid at the specified index.
   * @param index The index at which to add the column.
   * @param type The type of the column to add.
   * @param value The value associated with the column type.
   *
   * @example
   * ```js
   * gridLayout.addColumnAtIndex(1, 'auto');
   * ```
   */
  addColumnAtIndex(index: number, type: TrackType, value: number): void;
  /**
   * Removes a row from the grid at the specified index.
   * @param index The index of the row to remove.
   *
   * @example
   * ```js
   * gridLayout.removeRow(2);
   * ```
   */
  removeRow(index: number): void;
  /**
   * Removes a column from the grid at the specified index.
   * @param index The index of the column to remove.
   *
   * @example
   * ```js
   * gridLayout.removeColumn(3);
   * ```
   */
  removeColumn(index: number): void;
  /**
   * Sets the properties of a column at the specified index.
   * @param index The index of the column to set.
   * @param type The type of the column.
   * @param value The value associated with the column type (optional).
   *
   * @example
   * ```js
   * gridLayout.setColumn(0, 'fixed', 200);
   * ```
   */
  setColumn(index: number, type: TrackType, value?: number): void;
  /**
   * Sets the properties of a row at the specified index.
   * @param index The index of the row to set.
   * @param type The type of the row.
   * @param value The value associated with the row type (optional).
   *
   * @example
   * ```js
   * gridLayout.setRow(1, 'flex');
   * ```
   */
  setRow(index: number, type: TrackType, value?: number): void;

  /**
   * Appends a child element to the grid at the specified row and column.
   * @param child The child element to append.
   * @param row The row index where the child will be placed.
   * @param column The column index where the child will be placed.
   *
   * @example
   * ```js
   * gridLayout.appendChild(childShape, 0, 1);
   * ```
   */
  appendChild(child: Shape, row: number, column: number): void;
}

/**
 * Represents a group of shapes in Penpot.
 * This interface extends `ShapeBase` and includes properties and methods specific to groups.
 */
export interface Group extends ShapeBase {
  /**
   * The type of the shape, which is always 'group' for groups.
   */
  readonly type: 'group';

  /**
   * When true the group is a 3D space, as CSS `transform-style:
   * preserve-3d`: its children keep their depth (`translateZ`) and tilt
   * (`rotateX`, `rotateY`) in one space with its own, seen once with the
   * perspective of its board, and draw back to front. An opacity, a blur,
   * a shadow or a mask flattens it. Penpot Motion.
   */
  preserve3d: boolean;

  // Container Properties
  /**
   * The children shapes contained within the group.
   */
  readonly children: Shape[];
  /**
   * Appends a child shape to the group.
   * @param child The child shape to append.
   *
   * @example
   * ```js
   * group.appendChild(childShape);
   * ```
   */
  appendChild(child: Shape): void;
  /**
   * Inserts a child shape at the specified index within the group.
   * @param index The index at which to insert the child shape.
   * @param child The child shape to insert.
   *
   * @example
   * ```js
   * group.insertChild(0, childShape);
   * ```
   */
  insertChild(index: number, child: Shape): void;

  /**
   * Checks if the group is currently a mask.
   * A mask defines a clipping path for its child shapes.
   */
  isMask(): boolean;

  /**
   * Converts the group into a mask.
   */
  makeMask(): void;
  /**
   * Removes the mask from the group.
   */
  removeMask(): void;

  /**
   * How the mask of the group keeps its other layers (see `MaskMode`), or
   * `null` when the group is not a mask. Setting it on a group that is not
   * a mask is not valid. Requires `content:write` permission to set.
   *
   * @example
   * ```js
   * group.makeMask();
   * group.maskMode = 'luma-inverted';
   * ```
   */
  maskMode: MaskMode | null;
}

/**
 * Represents a board guide in Penpot.
 * This type can be one of several specific board guide types: column, row, or square.
 */
export type Guide = GuideColumn | GuideRow | GuideSquare;

/**
 * Represents a board guide for columns in Penpot.
 * This interface includes properties for defining the type, visibility, and parameters of column guides within a board.
 */
export interface GuideColumn {
  /**
   * The type of the guide, which is always 'column' for column guides.
   */
  readonly type: 'column';
  /**
   * Specifies whether the column guide is displayed.
   */
  readonly display: boolean;
  /**
   * The parameters defining the appearance and layout of the column guides.
   */
  readonly params: GuideColumnParams;
}

/**
 * Represents parameters for board guide columns in Penpot.
 * This interface includes properties for defining the appearance and layout of column guides within a board.
 */
export interface GuideColumnParams {
  /**
   * The color configuration for the column guides.
   */
  readonly color: { color: string; opacity: number };
  /**
   * The optional alignment type of the column guides.
   * - 'stretch': Columns stretch to fit the available space.
   * - 'left': Columns align to the left.
   * - 'center': Columns align to the center.
   * - 'right': Columns align to the right.
   */
  readonly type?: 'stretch' | 'left' | 'center' | 'right';
  /**
   * The optional size of each column.
   */
  readonly size?: number;
  /**
   * The optional margin between the columns and the board edges.
   */
  readonly margin?: number;
  /**
   * The optional length of each item within the columns.
   */
  readonly itemLength?: number;
  /**
   * The optional gutter width between columns.
   */
  readonly gutter?: number;
}

/**
 * Represents a board guide for rows in Penpot.
 * This interface includes properties for defining the type, visibility, and parameters of row guides within a board.
 */
export interface GuideRow {
  /**
   * The type of the guide, which is always 'row' for row guides.
   */
  readonly type: 'row';
  /**
   * Specifies whether the row guide is displayed.
   */
  readonly display: boolean;
  /**
   * The parameters defining the appearance and layout of the row guides.
   * Note: This reuses the same parameter structure as column guides.
   */
  readonly params: GuideColumnParams;
}

/**
 * Represents a board guide for squares in Penpot.
 * This interface includes properties for defining the type, visibility, and parameters of square guides within a board.
 */
export interface GuideSquare {
  /**
   * The type of the guide, which is always 'square' for square guides.
   */
  readonly type: 'square';
  /**
   * Specifies whether the square guide is displayed.
   */
  readonly display: boolean;
  /**
   * The parameters defining the appearance and layout of the square guides.
   */
  readonly params: GuideSquareParams;
}

/**
 * Represents parameters for board guide squares in Penpot.
 * This interface includes properties for defining the appearance and size of square guides within a board.
 */
export interface GuideSquareParams {
  /**
   * The color configuration for the square guides.
   */
  readonly color: { color: string; opacity: number };
  /**
   * The optional size of each square guide.
   */
  readonly size?: number;
}

/**
 * This object allows to access to some history functions
 */
export interface HistoryContext {
  /**
   * Starts an undo block. All operations done inside this block will be undone together until
   * a call to `undoBlockFinish` is called.
   * @returns the block identifier
   */
  undoBlockBegin(): Symbol;

  /**
   * Ends the undo block started with `undoBlockBegin`
   * @param blockId is the id returned by `undoBlockBegin`
   *
   * @example
   * ```js
   * historyContext.undoBlockFinish(blockId);
   * ```
   */
  undoBlockFinish(blockId: Symbol): void;
}

/**
 * Represents an image shape in Penpot.
 * This interface extends `ShapeBase` and includes properties specific to image shapes.
 * @deprecated Image shapes exist only for backward compatibility with old files.
 *   New images are embedded in a `Fill` via its `fillImage` (an `ImageData`).
 */
export interface Image extends ShapeBase {
  /**
   * The type of the shape, which is always 'image' for image shapes.
   */
  readonly type: 'image';

  /**
   * The fills applied to the shape.
   */
  fills: Fill[];
}

/**
 * Represents image data in Penpot.
 * This includes properties for defining the image's dimensions, metadata, and aspect ratio handling.
 */
export type ImageData = {
  /**
   * The optional name of the image.
   */
  readonly name?: string;
  /**
   * The width of the image.
   */
  readonly width: number;
  /**
   * The height of the image.
   */
  readonly height: number;
  /**
   * The optional media type of the image (e.g., 'image/png', 'image/jpeg').
   */
  readonly mtype?: string;
  /**
   * The unique identifier for the image.
   */
  readonly id: string;
  /**
   * Whether to keep the aspect ratio of the image when resizing.
   * Defaults to false if omitted.
   */
  readonly keepAspectRatio?: boolean;

  /**
   * Returns the image data as a byte array.
   */
  data(): Promise<Uint8Array>;
};

/**
 * Penpot allows you to prototype interactions by connecting boards, which can act as screens.
 */
export interface Interaction {
  /**
   * The shape that owns the interaction
   */
  readonly shape?: Shape;

  /**
   * The user action that will start the interaction.
   */
  trigger: Trigger;

  /**
   * Time in **milliseconds** after the action will happen. Only applies to `after-delay` triggers.
   */
  delay?: number | null;

  /**
   * The action that will execute after the trigger happens.
   */
  action: Action;

  /**
   * Removes the interaction
   */
  remove(): void;
}

/**
 * Properties for defining the layout of a cell in Penpot.
 */
export interface LayoutCellProperties {
  /**
   * The row index of the cell.
   * This value is optional and indicates the starting row of the cell.
   */
  row?: number;

  /**
   * The number of rows the cell should span.
   * This value is optional and determines the vertical span of the cell.
   */
  rowSpan?: number;

  /**
   * The column index of the cell.
   * This value is optional and indicates the starting column of the cell.
   */
  column?: number;

  /**
   * The number of columns the cell should span.
   * This value is optional and determines the horizontal span of the cell.
   */
  columnSpan?: number;

  /**
   * The name of the grid area that this cell belongs to.
   * This value is optional and can be used to define named grid areas.
   */
  areaName?: string;

  /**
   * The positioning mode of the cell.
   * This value can be 'auto', 'manual', or 'area' and determines how the cell is positioned within the layout.
   */
  position?: 'auto' | 'manual' | 'area';
}

/**
 * Properties for defining the layout of a child element in Penpot.
 */
export interface LayoutChildProperties {
  /**
   * Specifies whether the child element is positioned absolutely.
   * When set to true, the element is taken out of the normal document flow and positioned relative to its nearest positioned ancestor.
   */
  absolute: boolean;

  /**
   * Defines the stack order of the child element
   * Elements with a higher zIndex will be displayed in front of those with a lower zIndex.
   */
  zIndex: number;

  /**
   * Determines the horizontal sizing behavior of the child element
   * - 'auto': The width is determined by the content.
   * - 'fill': The element takes up the available width.
   * - 'fix': The width is fixed.
   */
  horizontalSizing: 'auto' | 'fill' | 'fix';

  /**
   * Determines the vertical sizing behavior of the child element.
   * - 'auto': The height is determined by the content.
   * - 'fill': The element takes up the available height.
   * - 'fix': The height is fixed.
   */
  verticalSizing: 'auto' | 'fill' | 'fix';

  /**
   * Aligns the child element within its container.
   * - 'auto': Default alignment.
   * - 'start': Aligns the element at the start of the container.
   * - 'center': Centers the element within the container.
   * - 'end': Aligns the element at the end of the container.
   * - 'stretch': Stretches the element to fill the container.
   */
  alignSelf: 'auto' | 'start' | 'center' | 'end' | 'stretch';

  /**
   * Sets the horizontal margin of the child element.
   * This is the space on the left and right sides of the element.
   */
  horizontalMargin: number;

  /**
   * Sets the vertical margin of the child element.
   * This is the space on the top and bottom sides of the element.
   */
  verticalMargin: number;

  /**
   * Sets the top margin of the child element.
   * This is the space above the element.
   */
  topMargin: number;

  /**
   * Sets the right margin of the child element.
   * This is the space to the right of the element.
   */
  rightMargin: number;

  /**
   * Sets the bottom margin of the child element.
   * This is the space below the element.
   */
  bottomMargin: number;

  /**
   * Sets the left margin of the child element.
   * This is the space to the left of the element.
   */
  leftMargin: number;

  /**
   * The `marginType` property specifies how the four margin values are applied.
   * It can be one of the following values:
   * - 'simple': the vertical and horizontal margins are mirrored across both sides.
   * - 'multiple': each of the four sides (top, right, bottom, left) is honoured independently.
   */
  marginType: 'simple' | 'multiple';

  /**
   * Defines the maximum width of the child element.
   * If set to null, there is no maximum width constraint.
   */
  maxWidth: number | null;

  /**
   * Defines the maximum height of the child element.
   * If set to null, there is no maximum height constraint.
   */
  maxHeight: number | null;
  /**
   * Defines the minimum width of the child element.
   * If set to null, there is no minimum width constraint.
   */
  minWidth: number | null;

  /**
   * Defines the minimum height of the child element.
   * If set to null, there is no minimum height constraint.
   */
  minHeight: number | null;
}

/**
 * Represents a library in Penpot, containing colors, typographies, and components.
 */
export interface Library extends PluginData {
  /**
   * The unique identifier of the library.
   */
  readonly id: string;

  /**
   * The name of the library.
   */
  readonly name: string;

  /**
   * An array of color elements in the library.
   * @example
   * ```js
   * console.log(penpot.library.local.colors);
   * ```
   */
  readonly colors: LibraryColor[];

  /**
   * An array of typography elements in the library.
   */
  readonly typographies: LibraryTypography[];

  /**
   * The animation styles of the library (Penpot Motion): lists of preset
   * animations that layers play through uses of them, see
   * `Timeline.applyAnimationStyle`.
   */
  readonly animationStyles: LibraryAnimationStyle[];

  /**
   * An array of component elements in the library.
   * @example
   * ```js
   * console.log(penpot.library.local.components);
   */
  readonly components: LibraryComponent[];

  /**
   * Deleted components, including individual deleted variants.
   * Use a component's `restore()` method to restore it.
   */
  readonly deletedComponents: LibraryComponent[];

  /**
   * A catalog of Design Tokens in the library.
   *
   * See `TokenCatalog` type to see usage.
   */
  readonly tokens: TokenCatalog;

  /**
   * Creates a new color element in the library.
   * @return Returns a new `LibraryColor` object representing the created color element.
   *
   * @example
   * ```js
   * const newColor = penpot.library.local.createColor();
   * console.log(newColor);
   * ```
   */
  createColor(): LibraryColor;

  /**
   * Creates a new typography element in the library.
   * @return Returns a new `LibraryTypography` object representing the created typography element.
   *
   * @example
   * ```js
   * const newTypography = library.createTypography();
   * ```
   */
  createTypography(): LibraryTypography;

  /**
   * Creates an animation style in the library of the file itself, named
   * "Animation style", from the preset animations and keyframe entries it
   * plays (1 to 32, see `AnimationStyleEntry`). Needs Motion (the
   * `animation/v1` feature).
   * @param entries The preset animations and keyframe entries of the
   * style, their `start` counted from the start of the style.
   * @return Returns the new `LibraryAnimationStyle`.
   *
   * @example
   * ```js
   * const style = penpot.library.local.createAnimationStyle([
   *   { type: 'fade', duration: 400 },
   *   { type: 'move', duration: 400, offsetY: 40 },
   * ]);
   * style.name = 'Card enter';
   * ```
   */
  createAnimationStyle(entries: AnimationStyleEntry[]): LibraryAnimationStyle;

  /**
   * Creates a new component element in the library using the provided shapes.
   * @param shapes An array of `Shape` objects representing the shapes to be included in the component.
   * @return Returns a new `LibraryComponent` object representing the created component element.
   *
   * @example
   * ```js
   * const newComponent = penpot.library.local.createComponent([shape1, shape2]);
   * ```
   */
  createComponent(shapes: Shape[]): LibraryComponent;
}

/**
 * Represents a color element from a library in Penpot.
 * This interface extends `LibraryElement` and includes properties specific to color elements.
 */
export interface LibraryColor extends LibraryElement {
  /**
   * The color value of the library color.
   */
  color?: string;

  /**
   * The opacity value of the library color.
   */
  opacity?: number;

  /**
   * The gradient value of the library color, if it's a gradient.
   */
  gradient?: Gradient;

  /**
   * The image data of the library color, if it's an image fill.
   */
  image?: ImageData;

  /**
   * Converts the library color into a fill object.
   * @return Returns a `Fill` object representing the color as a fill.
   *
   * @example
   * ```js
   * const fill = libraryColor.asFill();
   * ```
   */
  asFill(): Fill;
  /**
   * Converts the library color into a stroke object.
   * @return Returns a `Stroke` object representing the color as a stroke.
   *
   * @example
   * ```js
   * const stroke = libraryColor.asStroke();
   * ```
   */
  asStroke(): Stroke;
}

/**
 * An animation style of a library (Penpot Motion): a list of preset
 * animations that layers play through uses of it (see
 * `Timeline.applyAnimationStyle`). A use plays a copy of the entries and
 * follows the edits of the style, in its file and in the files that use
 * its library. The style changes only in the file it belongs to, and
 * needs Motion (the `animation/v1` feature) to change.
 */
export interface LibraryAnimationStyle extends LibraryElement {
  /**
   * What the style is for.
   */
  description: string;

  /**
   * The preset animations and keyframe entries the style plays, their
   * `start` counted from the start of the style. Setting them replaces
   * them all (1 to 32, with at most 256 keyframes), and every use follows:
   * a use as long as the style takes its new length, a stretched one keeps
   * its own.
   */
  entries: AnimationStyleEntry[];

  /**
   * How long the style plays, in milliseconds: the end of its last entry.
   */
  readonly duration: number;

  /**
   * Saves into the style what `shape`, a layer that uses it, plays now:
   * its preset animations, the entries of its uses of styles (this one
   * too) and its rows of keyframes that are neither hidden, locked nor
   * looping, which become keyframe entries (see `KeyframesStyleEntry`).
   * The use takes their place, still stretched as it was, and every use
   * of the style follows, in one undo step. The way to change the values
   * of keyframe entries: apply the style, add keyframes to the layer
   * (they play at once in their span, see `KeyframesStyleEntry`), then
   * update the style from it, which saves the row as it plays. Only a
   * style of the file itself, and a layer on the current page.
   *
   * @example
   * ```js
   * const use = timeline.applyAnimationStyle(card, style, 0);
   * timeline.addKeyframe(card, { property: 'rotation', time: 200, value: 12 });
   * style.updateFromShape(card);
   * ```
   */
  updateFromShape(shape: Shape): void;

  /**
   * Removes the style. Its uses turn into the preset animations they play,
   * and their keyframe entries into keyframes of their layers.
   */
  remove(): void;

  /**
   * Creates a copy of the style in the same library.
   * @return Returns the new `LibraryAnimationStyle`.
   */
  clone(): LibraryAnimationStyle;
}

/**
 * Represents a component element from a library in Penpot.
 * This interface extends `LibraryElement` and includes properties specific to component elements.
 */
export interface LibraryComponent extends LibraryElement {
  /**
   * The component annotation, or null when none is set.
   * Set null to delete it. Text can contain at most 300 characters.
   * Writing requires `library:write` permission and a live component in the
   * current file's library.
   */
  annotation: string | null;

  /**
   * Duplicates this component and its main instance in the current file's library.
   * Requires `library:write` permission. The component must not be deleted.
   */
  duplicate(): LibraryComponent;

  /**
   * Deletes this component and its main instance from the current file's library.
   * Requires `library:write` permission. Does nothing if already deleted.
   * Surviving copies keep their link so the component can be restored later.
   */
  remove(): void;

  /**
   * Restores this component if it has been deleted, including its main instance.
   * Leaves the current page, selection, zoom, and viewport unchanged.
   * Requires `library:write` permission. Does nothing if already restored.
   *
   * @example
   * ```js
   * penpot.selection[0].component()?.restore();
   * ```
   */
  restore(): void;

  /**
   * Creates an instance of the component.
   * @return Returns a `Shape` object representing the instance of the component.
   *
   * @example
   * ```js
   * const componentInstance = libraryComponent.instance();
   * ```
   */
  instance(): Shape;

  /**
   * @return Returns the reference to the main component shape.
   */
  mainInstance(): Shape;

  /**
   * Checks whether this component is a variant component.
   * If true, the component can be used as a `LibraryVariantComponent`,
   * which provides additional attributes (e.g. `variants`) and methods.
   */
  isVariant(): this is LibraryVariantComponent;

  /**
   * Creates a new Variant from this standard Component. It creates a VariantContainer, transforms this Component into a VariantComponent, duplicates it, and creates a
   * set of properties based on the component name and path.
   * Similar to doing it with the contextual menu or the shortcut on the Penpot interface
   */
  transformInVariant(): void;
}

/**
 * Represents a component element from a library in Penpot.
 * This interface extends `LibraryElement` and includes properties specific to component elements.
 */
export interface LibraryVariantComponent extends LibraryComponent {
  /**
   * Access to the Variant interface, for attributes and actions over the full Variant (not only this VariantComponent)
   */
  readonly variants: Variants | null;

  /**
   * A list of the variant props of this VariantComponent. Each property has a key and a value
   */
  readonly variantProps: { [property: string]: string };

  /**
   * If this VariantComponent has an invalid name, that doesn't follow the structure [property]=[value], [property]=[value]
   * this field stores that invalid name
   */
  variantError: string;

  /**
   * Creates a duplicate of the current VariantComponent on its Variant
   */
  addVariant(): void;

  /**
   * Sets the value of the variant property on the indicated position
   * @param pos The position of the property to update
   * @param value The new value of the property
   */
  setVariantProperty(pos: number, value: string): void;
}

/**
 * Represents the context of Penpot libraries, including both local and connected libraries.
 * This type contains references to the local library and an array of connected libraries.
 */
export type LibraryContext = {
  /**
   * The local library in the Penpot context.
   *
   * @example
   * ```js
   * const localLibrary = libraryContext.local;
   * ```
   */
  readonly local: Library;

  /**
   * An array of connected libraries in the Penpot context.
   *
   * @example
   * ```js
   * const connectedLibraries = libraryContext.connected;
   * ```
   */
  readonly connected: Library[];

  /**
   * Retrieves a summary of available libraries that can be connected to.
   * @return Returns a promise that resolves to an array of `LibrarySummary` objects representing available libraries.
   *
   * @example
   * ```js
   * const availableLibraries = await libraryContext.availableLibraries();
   * ```
   */
  availableLibraries(): Promise<LibrarySummary[]>;

  /**
   * Connects to a specific library identified by its ID.
   * @return Returns a promise that resolves to the `Library` object representing the connected library.
   * @param libraryId - The ID of the library to connect to.
   *
   * @example
   * ```js
   * const connectedLibrary = await libraryContext.connectLibrary('library-id');
   * ```
   */
  connectLibrary(libraryId: string): Promise<Library>;
};

/**
 * Represents an element in a Penpot library.
 * This interface provides information about a specific element in a library.
 */
export interface LibraryElement extends PluginData {
  /**
   * The unique identifier of the library element.
   */
  readonly id: string;

  /**
   * The unique identifier of the library to which the element belongs.
   */
  readonly libraryId: string;

  /**
   * The name of the library element.
   */
  name: string;

  /**
   * The path of the library element.
   */
  path: string;
}

/**
 * Represents a summary of a Penpot library.
 * This interface provides properties for summarizing various aspects of a Penpot library.
 */
export interface LibrarySummary {
  /**
   * The unique identifier of the library.
   */
  readonly id: string;

  /**
   * The name of the library.
   */
  readonly name: string;

  /**
   * The number of colors in the library.
   */
  readonly numColors: number;

  /**
   * The number of components in the library.
   */
  readonly numComponents: number;

  /**
   * The number of typographies in the library.
   */
  readonly numTypographies: number;
}

/**
 * Represents a typography element from a library in Penpot.
 * This interface extends `LibraryElement` and includes properties specific to typography elements.
 */
export interface LibraryTypography extends LibraryElement {
  /**
   * The unique identifier of the font used in the typography element.
   */
  fontId: string;

  /**
   * The font family of the typography element.
   */
  fontFamily: string;

  /**
   * The unique identifier of the font variant used in the typography element.
   */
  fontVariantId: string;

  /**
   * The font size of the typography element.
   */
  fontSize: string;

  /**
   * The font weight of the typography element.
   */
  fontWeight: string;

  /**
   * The font style of the typography element.
   */
  fontStyle?: 'normal' | 'italic' | null;

  /**
   * The line height of the typography element.
   */
  lineHeight: string;

  /**
   * The letter spacing of the typography element.
   */
  letterSpacing: string;

  /**
   * The text transform applied to the typography element.
   */
  textTransform?: 'uppercase' | 'capitalize' | 'lowercase' | null;

  /**
   * Applies the typography styles to a text shape.
   * @param shape The text shape to apply the typography styles to.
   *
   * @example
   * ```js
   * typographyElement.applyToText(textShape);
   * ```
   */
  applyToText(shape: Shape): void;

  /**
   * Applies the typography styles to a range of text within a text shape.
   * @param range Represents a range of text within a Text shape. This interface provides properties for styling and formatting text ranges.
   *
   * @example
   * ```js
   * typographyElement.applyToTextRange(textShape);
   * ```
   */
  applyToTextRange(range: TextRange): void;

  /**
   * Sets the font and optionally its variant for the typography element.
   * @param font - The font to set.
   * @param variant - The font variant to set (optional).
   *
   * @example
   * ```js
   * typographyElement.setFont(newFont, newVariant);
   * ```
   */
  setFont(font: Font, variant?: FontVariant): void;
}

/**
 * Proxy for the local storage. Only elements owned by the plugin
 * can be stored and accessed.
 * Warning: other plugins won't be able to access this information but
 * the user could potentially access the data through the browser information.
 */
export interface LocalStorage {
  /**
   * Retrieve the element with the given key
   * Requires the `allow:localstorage` permission.
   */
  getItem(key: string): string;

  /**
   * Set the data given the key. If the value already existed it
   * will be overridden. The value will be stored in a string representation.
   * Requires the `allow:localstorage` permission.
   */
  setItem(key: string, value: unknown): void;

  /**
   * Remove the value stored in the key.
   * Requires the `allow:localstorage` permission.
   */
  removeItem(key: string): void;

  /**
   * Return all the keys for the data stored by the plugin.
   * Requires the `allow:localstorage` permission.
   */
  getKeys(): string[];
}

/**
 * How the mask of a masked group, its bottom-most layer, keeps the other
 * layers of the group, as the track mattes of motion tools do:
 * - `alpha`: where the mask is, by its opacity (the default).
 * - `alpha-inverted`: where the mask is not.
 * - `luma`: where the mask is bright, by its luminance.
 * - `luma-inverted`: where the mask is dark.
 *
 * An inverted mask keeps what is outside it, so its group spans all its
 * layers instead of only the mask.
 */
export type MaskMode = 'alpha' | 'alpha-inverted' | 'luma' | 'luma-inverted';

/**
 * It takes the user from one board to the destination set in the interaction.
 */
export interface NavigateTo {
  /**
   * Type of action
   */
  readonly type: 'navigate-to';

  /**
   * Board to which the action targets
   */
  readonly destination: Board;

  /**
   * When true the scroll will be preserved.
   */
  readonly preserveScrollPosition?: boolean;

  /**
   * Animation displayed with this interaction.
   */
  readonly animation?: Animation;
}

/**
 * It opens a board right over the current board.
 */
export interface OpenOverlay extends OverlayAction {
  /**
   * The action type
   */
  readonly type: 'open-overlay';
}

/**
 * This action opens an URL in a new tab.
 */
export interface OpenUrl {
  /**
   * The action type
   */
  readonly type: 'open-url';
  /**
   * The URL to open when the action is executed
   */
  readonly url: string;
}

/**
 * Plays the timeline of the board the shape is on, in the view mode, from
 * where it shows to a marker of it, forwards or backwards, and pauses
 * there (Penpot Motion). A click on a "next" button of a carousel or a
 * coverflow moves it one marker on. While it plays to a marker, the next
 * and previous ones count from that marker, so a quick second click goes
 * on from there; past the first or last marker nothing happens.
 *
 * @example
 * ```js
 * const tl = board.timeline;
 * next.addInteraction('click', { type: 'play-to-marker', step: 'next' });
 * first.addInteraction('click', { type: 'play-to-marker', markerId: tl.markers[0].id });
 * ```
 */
export interface PlayToMarker {
  /**
   * The action type
   */
  readonly type: 'play-to-marker';
  /**
   * The id of the marker it plays to (see `Marker.id`). When not given, it
   * plays to the next or previous marker, as `step` says.
   */
  readonly markerId?: string;
  /**
   * Without `markerId`: whether it plays to the next marker (by default)
   * or the previous one.
   */
  readonly step?: 'next' | 'previous';
}

/**
 * Base type for the actions "open-overlay" and "toggle-overlay" that share most of their properties
 */
export interface OverlayAction {
  /**
   * Overlay board that will be opened.
   */
  readonly destination: Board;

  /**
   * Base shape to which the overlay will be positioned taking constraints into account.
   */
  readonly relativeTo?: Shape;

  /**
   * Positioning of the overlay.
   */
  readonly position?:
    | 'manual'
    | 'center'
    | 'top-left'
    | 'top-right'
    | 'top-center'
    | 'bottom-left'
    | 'bottom-right'
    | 'bottom-center';

  /**
   * For `position = 'manual'` the location of the overlay.
   */
  readonly manualPositionLocation?: Point;

  /**
   * When true the overlay will be closed when clicking outside
   */
  readonly closeWhenClickOutside?: boolean;

  /**
   * When true a background will be added to the overlay.
   */
  readonly addBackgroundOverlay?: boolean;

  /**
   * Animation displayed with this interaction.
   */
  readonly animation?: Animation;
}

/**
 * Page represents a page in the Penpot application.
 * It includes properties for the page's identifier and name, as well as methods for managing shapes on the page.
 */
export interface Page extends PluginData {
  /**
   * The `id` property is a unique identifier for the page.
   */
  readonly id: string;
  /**
   * The `name` property is the name of the page.
   */
  name: string;

  /**
   * The ruler guides attached to the page
   */
  readonly rulerGuides: RulerGuide[];

  /**
   * The keyframe animations (Penpot Motion) of the boards of the page, one
   * per animated board.
   */
  readonly timelines: Timeline[];

  /**
   * The root shape of the current page. Will be the parent shape of all the shapes inside the document.
   * Requires `content:read` permission.
   */
  readonly root: Shape;

  /**
   * Removes the page from the file. The last remaining page of the file
   * cannot be removed. If the removed page is the active one, another page
   * is activated.
   * Requires `content:write` permission.
   *
   * @example
   * ```js
   * const page = penpot.createPage();
   * page.remove();
   * ```
   */
  remove(): void;

  /**
   * Retrieves a shape by its unique identifier.
   * @param id The unique identifier of the shape.
   *
   * @example
   * ```js
   * const shape = penpot.currentPage.getShapeById('shapeId');
   * ```
   */
  getShapeById(id: string): Shape | null;

  /**
   * Finds all shapes on the page.
   * Optionally it gets a criteria object to search for specific criteria
   * @param criteria
   * @example
   * ```js
   * const shapes = penpot.currentPage.findShapes({ name: 'exampleName' });
   * ```
   */
  findShapes(criteria?: {
    name?: string;
    nameLike?: string;
    type?:
      | 'board'
      | 'group'
      | 'boolean'
      | 'rectangle'
      | 'path'
      | 'text'
      | 'ellipse'
      | 'svg-raw'
      | 'image';
  }): Shape[];

  /**
   * The interaction flows defined for the page.
   */
  readonly flows: Flow[];

  /**
   * Creates a new flow in the page.
   * @param name the name identifying the flow
   * @param board the starting board for the current flow
   *
   * @example
   * ```js
   * const flow = penpot.currentPage.createFlow('exampleFlow', board);
   * ```
   */
  createFlow(name: string, board: Board): Flow;

  /**
   * Removes the flow from the page
   * @param flow the flow to be removed from the page
   */
  removeFlow(flow: Flow): void;

  /**
   * Creates a new ruler guide.
   * @param orientation `horizontal` or `vertical`
   * @param value the position of the guide in absolute coordinates
   * @param board if provided, the guide will be attached to the board
   */
  addRulerGuide(
    orientation: RulerGuideOrientation,
    value: number,
    board?: Board,
  ): RulerGuide;

  /**
   * Removes the `guide` from the current page.
   */
  removeRulerGuide(guide: RulerGuide): void;

  /**
   * Creates a new comment thread in the `position`.
   * Returns the thread created.
   * Requires the `comment:write` permission.
   */
  addCommentThread(content: string, position: Point): Promise<CommentThread>;

  /**
   * Removes the comment thread.
   * Requires the `comment:write` permission.
   */
  removeCommentThread(commentThread: CommentThread): Promise<void>;

  /**
   * Find all the comments that match the criteria.
   * - `onlyYours`: if `true` will return the threads where the current
   *                user has engaged.
   * - `showResolved`: by default resolved comments will be hidden. If `true`
   *                   the resolved will be returned.
   * Requires the `comment:read` or `comment:write` permission.
   */
  findCommentThreads(criteria?: {
    onlyYours: boolean;
    showResolved: boolean;
  }): Promise<CommentThread[]>;
}

/**
 * Represents a path shape in Penpot.
 * This interface extends `ShapeBase` and includes properties and methods specific to paths.
 */
export interface Path extends ShapeBase {
  /**
   * The type of the shape, which is always 'path' for path shapes.
   */
  readonly type: 'path';
  /**
   * Converts the path shape to its path data representation.
   * @return Returns the path data (d attribute) as a string.
   * @deprecated Use the `d` attribute
   */
  toD(): string;

  /**
   * The content of the path shape, defined as the path string.
   * @deprecated Use either `d` or `commands`.
   */
  content: string;

  /**
   * The content of the path shape, defined as the path string.
   */
  d: string;

  /**
   * The content of the path shape, defined as an array of path commands.
   */
  commands: Array<PathCommand>;

  /**
   * The fills applied to the shape.
   */
  fills: Fill[];
}

/**
 * Represents a path command in Penpot.
 * This interface includes a property for defining the type of command.
 */
interface PathCommand {
  /**
   * The type of path command.
   * Possible values include:
   * - 'M' or 'move-to': Move to a new point.
   * - 'Z' or 'close-path': Close the current path.
   * - 'L' or 'line-to': Draw a straight line to a new point.
   * - 'H' or 'line-to-horizontal': Draw a horizontal line to a new point.
   * - 'V' or 'line-to-vertical': Draw a vertical line to a new point.
   * - 'C' or 'curve-to': Draw a cubic Bezier curve to a new point.
   * - 'S' or 'smooth-curve-to': Draw a smooth cubic Bezier curve to a new point.
   * - 'Q' or 'quadratic-bezier-curve-to': Draw a quadratic Bezier curve to a new point.
   * - 'T' or 'smooth-quadratic-bezier-curve-to': Draw a smooth quadratic Bezier curve to a new point.
   * - 'A' or 'elliptical-arc': Draw an elliptical arc to a new point.
   *
   * @example
   * ```js
   * const pathCommand: PathCommand = { command: 'M', params: { x: 0, y: 0 } };
   * ```
   */
  command:
    | 'M'
    | 'move-to'
    | 'Z'
    | 'close-path'
    | 'L'
    | 'line-to'
    | 'H'
    | 'line-to-horizontal'
    | 'V'
    | 'line-to-vertical'
    | 'C'
    | 'curve-to'
    | 'S'
    | 'smooth-curve-to'
    | 'Q'
    | 'quadratic-bezier-curve-to'
    | 'T'
    | 'smooth-quadratic-bezier-curve-to'
    | 'A'
    | 'elliptical-arc';

  /**
   * Optional parameters associated with the path command.
   */
  params?: {
    /**
     * The x-coordinate of the point (or endpoint).
     */
    x?: number;

    /**
     * The y-coordinate of the point (or endpoint).
     */
    y?: number;

    /**
     * The x-coordinate of the first control point for curves.
     */
    c1x?: number;

    /**
     * The y-coordinate of the first control point for curves.
     */
    c1y?: number;

    /**
     * The x-coordinate of the second control point for curves.
     */
    c2x?: number;

    /**
     * The y-coordinate of the second control point for curves.
     */
    c2y?: number;

    /**
     * The radius of the ellipse's x-axis.
     */
    rx?: number;

    /**
     * The radius of the ellipse's y-axis.
     */
    ry?: number;

    /**
     * The rotation angle of the ellipse's x-axis.
     */
    xAxisRotation?: number;

    /**
     * A flag indicating whether to use the larger arc.
     */
    largeArcFlag?: boolean;

    /**
     * A flag indicating the direction of the arc.
     */
    sweepFlag?: boolean;
  };
}

/**
 * Provides methods for managing plugin-specific data associated with a Penpot shape.
 */
export interface PluginData {
  /**
   * Retrieves the data for our own plugin, given a specific key.
   *
   * @param key The key for which to retrieve the data.
   * @return Returns the data associated with the key as a string.
   *
   * @example
   * ```js
   * const data = shape.getPluginData('exampleKey');
   * console.log(data);
   * ```
   */
  getPluginData(key: string): string;

  /**
   * Sets the plugin-specific data for the given key.
   *
   * @param key The key for which to set the data.
   * @param value The data to set for the key.
   *
   * @example
   * ```js
   * shape.setPluginData('exampleKey', 'exampleValue');
   * ```
   */
  setPluginData(key: string, value: string): void;

  /**
   * Retrieves all the keys for the plugin-specific data.
   *
   * @return Returns an array of strings representing all the keys.
   *
   * @example
   * ```js
   * const keys = shape.getPluginDataKeys();
   * console.log(keys);
   * ```
   */
  getPluginDataKeys(): string[];

  /**
   * If we know the namespace of an external plugin, this is the way to get their data.
   *
   * @param namespace The namespace for the shared data.
   * @param key The key for which to retrieve the data.
   * @return Returns the shared data associated with the key as a string.
   *
   * @example
   * ```js
   * const sharedData = shape.getSharedPluginData('exampleNamespace', 'exampleKey');
   * console.log(sharedData);
   * ```
   */
  getSharedPluginData(namespace: string, key: string): string;

  /**
   * Sets the shared plugin-specific data for the given namespace and key.
   *
   * @param namespace The namespace for the shared data.
   * @param key The key for which to set the data.
   * @param value The data to set for the key.
   *
   * @example
   * ```js
   * shape.setSharedPluginData('exampleNamespace', 'exampleKey', 'exampleValue');
   * ```
   */
  setSharedPluginData(namespace: string, key: string, value: string): void;

  /**
   * Retrieves all the keys for the shared plugin-specific data in the given namespace.
   *
   * @param namespace The namespace for the shared data.
   * @return Returns an array of strings representing all the keys in the namespace.
   *
   * @example
   * ```js
   * const sharedKeys = shape.getSharedPluginDataKeys('exampleNamespace');
   * console.log(sharedKeys);
   * ```
   */
  getSharedPluginDataKeys(namespace: string): string[];
}

/**
 * Point represents a point in 2D space, typically with x and y coordinates.
 */
export type Point = { readonly x: number; readonly y: number };

/**
 * It takes back to the last board shown.
 */
export interface PreviousScreen {
  /**
   * The action type
   */
  readonly type: 'previous-screen';
}

/**
 * Push animation
 */
export interface Push {
  /**
   * Type of the animation
   */
  readonly type: 'push';

  /**
   * Direction for the push animation
   */
  readonly direction: 'right' | 'left' | 'up' | 'down';

  /**
   * Duration of the animation effect
   */
  readonly duration: number;

  /**
   * Function that the push effect will follow for the interpolation.
   * Defaults to `linear`
   */
  readonly easing?: 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out';
}

/**
 * Represents a rectangle shape in Penpot.
 * This interface extends `ShapeBase` and includes properties specific to rectangles.
 */
export interface Rectangle extends ShapeBase {
  /**
   * The type of the shape, which is always 'rect' for rectangle shapes.
   */
  readonly type: 'rectangle';

  /**
   * The fills applied to the shape.
   */
  fills: Fill[];
}

/**
 * Represents a ruler guide. These are horizontal or vertical lines that can be
 * used to position elements in the UI.
 */
export interface RulerGuide {
  /**
   * `orientation` indicates whether the ruler is either `horizontal` or `vertical`
   */
  readonly orientation: RulerGuideOrientation;

  /**
   * `position` is the position in the axis in absolute coordinates. If this is a board
   * guide it will return the position relative to the board.
   */
  position: number;

  /**
   * If the guide is attached to a board this will retrieve the board shape
   */
  board?: Board;

  /**
   * Removes the guide from its page.
   */
  remove(): void;
}

/**
 * The possible orientations for a ruler guide: 'horizontal' or 'vertical'.
 */
export type RulerGuideOrientation = 'horizontal' | 'vertical';

/**
 * The keyframe animation of a board (Penpot Motion). The board and the
 * layers inside it animate over `duration` milliseconds in two ways:
 * - Keyframes: the value of a property of a layer at a time; between two
 *   keyframes the value goes from one to the other, eased as the first one
 *   says.
 * - Preset animations: an effect (fade, move, scale or rotate) over a span
 *   of time that stays editable, like an entrance or an exit.
 *
 * It can play sound too: audio clips, see `audioClips`.
 *
 * Changing it needs the Motion feature (`animation/v1`) and the
 * `content:write` permission. Every change can be undone.
 *
 * @example
 * ```js
 * const board = penpot.selection[0];
 * const [title, button] = board.children;
 * const timeline = board.addTimeline({ duration: 1200 });
 * timeline.addKeyframe(title, { property: 'x', time: 0, value: -200 });
 * timeline.addKeyframe(title, { property: 'x', time: 800, value: 40, easing: 'ease-out' });
 * timeline.addAnimation(button, { type: 'fade', start: 600, duration: 400 });
 * ```
 */
export interface Timeline {
  /**
   * The board the timeline animates.
   */
  readonly board: Board;

  /**
   * The name of the animation.
   */
  name: string;

  /**
   * How long the animation plays, in milliseconds. Adding a keyframe or a
   * preset animation past it makes it longer.
   */
  duration: number;

  /**
   * What playback does at the end: stop there (`once`), start over (`loop`)
   * or play backwards and on (`ping-pong`).
   */
  playback: TimelinePlayback;

  /**
   * Whether the prototype (view mode) plays the animation from its start
   * when it shows the board: when view mode opens on it, or a link or the
   * arrows lead to it. `true` by default; with `false` the board shows at
   * rest until it is played. The animated component copies in the board
   * go on looping on their own clock once its animation has ended.
   */
  autoplay: boolean;

  /**
   * The keyframes of the board and the layers inside it, by time.
   */
  readonly keyframes: Keyframe[];

  /**
   * The preset animations of the board and the layers inside it, by start.
   * The uses of animation styles are in `appliedStyles`.
   */
  readonly animations: PresetAnimation[];

  /**
   * The uses of animation styles by the board and the layers inside it,
   * by start, see `applyAnimationStyle`.
   */
  readonly appliedStyles: AppliedAnimationStyle[];

  /**
   * The markers of the timeline, by time: named moments that drags snap to
   * and the playhead jumps between. A Lottie export names the stretch from
   * each to the next one (or to the end), so a player can play it by name.
   */
  readonly markers: Marker[];

  /**
   * The audio clips of the timeline, by lane and start: the sound files the
   * board plays with its animation, in the workspace, in view mode and in
   * its video exports. A clip plays only within `duration`: adding or
   * moving one never makes the timeline longer. The animated component
   * copies in the board play the clips of their mains as well (see
   * `Board.animatedCopies`), which are not listed here.
   */
  readonly audioClips: AudioClip[];

  /**
   * Adds a keyframe of `shape`, the board or a layer inside it. A keyframe
   * of the same property at the same time takes its place.
   * @param shape The board or a layer inside it.
   * @param keyframe The property, time, value and easing of the keyframe.
   *
   * @example
   * ```js
   * timeline.addKeyframe(shape, { property: 'opacity', time: 0, value: 0 });
   * timeline.addKeyframe(shape, { property: 'opacity', time: 500, value: 1 });
   * timeline.addKeyframe(shape, { property: 'fillColor', time: 500, value: '#ff6600', index: 0 });
   * ```
   */
  addKeyframe(shape: Shape, keyframe: KeyframeProps): Keyframe;

  /**
   * Adds a preset animation of `shape`, the board or a layer inside it.
   * @param shape The board or a layer inside it.
   * @param animation The type, timing and settings of the animation.
   *
   * @example
   * ```js
   * timeline.addAnimation(shape, { type: 'move', start: 0, duration: 400, offsetY: 40 });
   * timeline.addAnimation(shape, { type: 'fade', direction: 'out', start: 1600, duration: 300 });
   * timeline.addAnimation(text, { type: 'fade', duration: 1200, split: 'word', spread: 0.6 });
   * ```
   */
  addAnimation(shape: Shape, animation: PresetAnimationProps): PresetAnimation;

  /**
   * Adds the preset animations of an animation style to `shape`, starting
   * at `start` milliseconds (0 by default), and returns them.
   * @param shape The board or a layer inside it.
   * @param style The style, like `slide-up` (a fade and a move up) or `pop`.
   * The text styles (`typewriter`, `rise-words`, `cascade-lines`,
   * `pop-letters`, `flip-letters`) go on text layers only.
   * @param start When they start, in milliseconds, before 0 too.
   */
  addAnimationStyle(
    shape: Shape,
    style: AnimationStyle,
    start?: number,
  ): PresetAnimation[];

  /**
   * Applies an animation style of the file or of one of its libraries to
   * `shape`, from `start` milliseconds (0 by default): a use that plays a
   * copy of the entries of the style and follows its edits. The timeline
   * grows to fit it.
   * @param shape The board or a layer inside it.
   * @param style A style of `penpot.library.local` or of a connected
   * library.
   * @param start When it starts, in milliseconds, before 0 too.
   *
   * @example
   * ```js
   * const [style] = penpot.library.local.animationStyles;
   * board.children.forEach((card, i) => {
   *   timeline.applyAnimationStyle(card, style, i * 80);
   * });
   * ```
   */
  applyAnimationStyle(
    shape: Shape,
    style: LibraryAnimationStyle,
    start?: number,
  ): AppliedAnimationStyle;

  /**
   * Copies the animation of `shape` onto `targets`, as copying an animation
   * on the canvas and pasting it onto other layers does. Each target moves,
   * turns and resizes from where it rests, by as much as `shape` does, in
   * the timeline of the board it is in (a board without one gets one). What
   * plays is copied: hidden keyframe rows and preset animations are not.
   * Keyframes of a fill, stroke, shadow, blur or corner radius a target does
   * not have are left out. So are targets outside any board, and `shape`
   * itself. One undo step undoes it all.
   * @param shape The board or a layer inside it.
   * @param targets Layers of the same page, in any board.
   * @param options What it copies and how it pastes it.
   *
   * @example
   * ```js
   * const [first, ...rest] = board.children;
   * timeline.addAnimationStyle(first, 'slide-up');
   * timeline.copyAnimation(first, rest, { stagger: 80 });
   * ```
   */
  copyAnimation(
    shape: Shape,
    targets: Shape[],
    options?: CopyAnimationOptions,
  ): CopyAnimationResult | null;

  /**
   * How the board is exported as an animation, as the export panel sets
   * it and offers it again. Reading it gives all the settings; setting it
   * changes the ones given.
   *
   * @example
   * ```js
   * timeline.exportSettings = { format: 'gif', fps: 15, size: 'w1920' };
   * ```
   */
  exportSettings: TimelineExportSettings;

  /**
   * Adds a marker. The timeline grows to hold it.
   * @param marker Its time and name (`Marker N` by default).
   *
   * @example
   * ```js
   * timeline.addMarker({ time: 0, name: 'intro' });
   * timeline.addMarker({ time: 800, name: 'loop' });
   * ```
   */
  addMarker(marker: MarkerProps): Marker;

  /**
   * Adds a clip that plays an audio file the plugin has as bytes, as
   * dropping the file on the board does: the file is decoded in the
   * browser first, then uploaded to the file and played by the new clip,
   * whole, from `start`. It rejects a file of another type, one the
   * browser cannot decode or one that lasts longer than 5 minutes, before
   * uploading anything; the server takes files up to 100 MB. The timeline
   * keeps its duration: set `duration` to hear all of a longer clip.
   * @param name The name of the file, shown on the clip.
   * @param data The bytes of the file.
   * @param mimeType Its type: `audio/mpeg` (MP3), `audio/wav`, `audio/ogg`,
   * `audio/mp4` (M4A), `audio/webm` or `audio/flac`.
   * @param props Where it plays from.
   * @return A promise of the new clip.
   *
   * @example
   * ```js
   * const response = await fetch('https://example.com/beat.mp3');
   * const data = new Uint8Array(await response.arrayBuffer());
   * const clip = await timeline.addAudioClip('beat.mp3', data, 'audio/mpeg', { start: 500 });
   * clip.fadeOut = 400;
   * ```
   */
  addAudioClip(
    name: string,
    data: Uint8Array | ArrayBuffer,
    mimeType: string,
    props?: AudioClipProps,
  ): Promise<AudioClip>;

  /**
   * Runs `callback` and records what it changes of `shapes` as keyframes at
   * `time`, as motion mode does with auto-keyframe (REC) on. A property
   * that is animated gets a keyframe there; one that is not yet also gets
   * one at 0 with the value it had before, so the change plays. The changes
   * stay on the shapes. Only `shapes` are recorded: moving a group also
   * moves its layers, and those follow the group.
   * @param time When the changes happen, in milliseconds.
   * @param shapes The board or layers inside it that `callback` changes.
   * @param callback Changes `shapes`, like `shape.x = 100`.
   *
   * @example
   * ```js
   * timeline.record(600, [card], () => {
   *   card.y = card.y - 40;
   *   card.opacity = 1;
   * });
   * ```
   */
  record(time: number, shapes: Shape[], callback: () => void): void;

  /**
   * The value the animation gives `property` of `shape` at `time`, or `null`
   * when it does not animate it.
   * @param shape The board or a layer inside it.
   * @param property The property.
   * @param time The time, in milliseconds.
   * @param index For fills, strokes and shadows: which one (0 by default).
   */
  valueAt(
    shape: Shape,
    property: AnimatableProperty,
    time: number,
    index?: number,
  ): number | string | null;

  /**
   * Removes the keyframes and preset animations of `shape`.
   */
  clear(shape: Shape): void;

  /**
   * The point `shape` turns and scales around, as a share of its box:
   * `{ x: 0.5, y: 0.5 }`, its center, until `setOrigin` sets another.
   * @param shape The board or a layer inside it.
   */
  originOf(shape: Shape): { x: number; y: number };

  /**
   * Sets the point `shape` turns and scales around, as a share of its box,
   * as the pivot of the Design tab does in motion mode: `{ x: 0, y: 0.5 }`
   * is the middle of its left side. Its `rotation`, `scaleX` and `scaleY`
   * keyframes play around it.
   * @param shape The board or a layer inside it.
   * @param origin Its `x` and `y`, each from 0 to 1.
   *
   * @example
   * ```js
   * timeline.setOrigin(door, { x: 0, y: 0.5 });
   * timeline.addKeyframe(door, { property: 'scaleX', time: 0, value: 1 });
   * timeline.addKeyframe(door, { property: 'scaleX', time: 600, value: 0 });
   * ```
   */
  setOrigin(shape: Shape, origin: { x: number; y: number }): void;

  /**
   * How the trim of the group `shape` (its `trimStart`, `trimEnd` and
   * `trimOffset` keyframes) draws the layers in it: `'together'`, each
   * as far along its own outline, or `'in-turn'`, one after another as if
   * their outlines made one, the topmost first. A group draws nothing
   * itself; its trim takes the place of those of the layers in it.
   * Penpot Motion.
   * @param shape A group inside the board.
   */
  trimModeOf(shape: Group): TrimMode;

  /**
   * Makes the trim of the group `shape` draw the layers in it `mode` (see
   * `trimModeOf`).
   * @param shape A group inside the board.
   * @param mode `'together'` or `'in-turn'`.
   *
   * Draw a group of lines as one, the topmost first:
   *
   * @example
   * ```js
   * timeline.setTrimMode(lines, 'in-turn');
   * timeline.addKeyframe(lines, { property: 'trimEnd', time: 0, value: 0 });
   * timeline.addKeyframe(lines, { property: 'trimEnd', time: 1200, value: 1 });
   * ```
   */
  setTrimMode(shape: Group, mode: TrimMode): void;

  /**
   * How `shape` follows another layer of the board, as the Follow section
   * of the Motion tab sets it, or `null` when it follows none.
   * @param shape A layer inside the board.
   */
  followOf(shape: Shape): Follow | null;

  /**
   * Makes `shape` follow another layer of the board, `follow.leader`: it
   * takes what the leader does from its first frame on (`from: 'start'`,
   * the default), or all that moves the leader from where it rests, as a
   * layer of a group moves with the group (`from: 'rest'`), `delay`
   * milliseconds later, after its own animation. It moves as the leader's
   * pivot moves, turns and scales around that pivot and, with `opacity`,
   * fades with the leader. `null` makes it follow none. The leader must
   * be another layer of the board, not one `shape` is in, holds or is
   * followed by, nor a layer of a component copy the board does not
   * animate itself (what a copy plays of its main moves no follower).
   * The SVG, CSS and code exports play a follow by keyframes made from
   * it. Lottie makes the layer a child of its leader's layer when it
   * follows with no delay, taking all that moves the leader and not its
   * opacity, from where the leader rests or a first frame at rest, and
   * the leader scales evenly and never flips; else by keyframes too.
   * @param shape A layer inside the board.
   * @param follow The leader and what the layer takes of it, or `null`.
   *
   * @example
   * ```js
   * timeline.setFollow(badge, { leader: card, delay: 100 });
   * timeline.setFollow(shadow, { leader: button, rotation: false });
   * timeline.setFollow(label, { leader: popIn, from: 'rest' });
   * ```
   */
  setFollow(shape: Shape, follow: FollowProps | null): void;

  /**
   * The links of the properties of `shape`, as the Links section of the
   * Motion tab sets them (see `link`).
   * @param shape A layer inside the board.
   */
  linksOf(shape: Shape): Link[];

  /**
   * Makes `link.property` of `shape` play `link.sourceProperty` of
   * another layer of the board, `link.source` (the shape itself too), as
   * the pick whip of After Effects with a linear expression: it plays
   * `rest + factor × (source − source at rest) + offset`, `delay`
   * milliseconds after the source, so linking never moves it at rest. It
   * takes the place of the property's own keyframes and loops where the
   * source loops; a chain of links plays through, a cycle plays nothing.
   * Every export plays it as keyframes made from the source's. Linking a
   * property linked already changes that link.
   * With a range (`link.input`, `link.output`) the source going through
   * its input takes the property through its output, along a curve, as
   * an After Effects `linear()` or `ease()` expression does.
   * @param shape A layer inside the board.
   * @param link The property, its source and how it plays it.
   *
   * The wheel turns 3.6° for each pixel the car moves, and the shadow
   * fades as the ball rises:
   *
   * @example
   * ```js
   * timeline.link(wheel, { property: 'rotation', source: car, sourceProperty: 'x', factor: 3.6 });
   * timeline.link(shadow, { property: 'opacity', source: ball, sourceProperty: 'y', factor: 0.004 });
   * ```
   */
  link(shape: Shape, link: LinkProps): void;

  /**
   * Makes `property` of `shape` play its own keyframes again (see `link`).
   * @param shape A layer inside the board.
   * @param property The property linked.
   */
  unlink(shape: Shape, property: LinkableProperty): void;

  /**
   * The goal `shape` reaches, as the Reach section of the Motion tab sets
   * it, or `null` when it reaches none.
   * @param shape A layer inside the board.
   */
  ikOf(shape: Shape): Ik | null;

  /**
   * Makes `shape`, the end of a chain of two follows (a hand that follows
   * the forearm, which follows the upper arm, each with no delay, taking
   * the position and rotation of its leader, see `setFollow`), reach
   * `ik.goal`: every frame the two links turn around their pivots so the
   * pivot of `shape` lands on the goal's, or points straight at a goal
   * out of reach, the elbow on the side `ik.bend` says. What follows a
   * link goes along. `null` makes it reach none. The goal must be another
   * layer of the board out of the chain, not in one of its layers nor
   * holding one, not led by the chain through follows and the goals of
   * other chains, nor a layer of a component copy the board does not
   * animate itself; a layer is in one chain only. The SVG, CSS, Lottie
   * and code exports play it by keyframes.
   * @param shape A layer inside the board, at the end of a chain of two follows.
   * @param ik The goal and how to reach it, or `null`.
   *
   * @example
   * ```js
   * timeline.setIk(hand, { goal: ball });
   * timeline.setIk(foot, { goal: footprint, bend: 'cw', end: 'keep' });
   * ```
   */
  setIk(shape: Shape, ik: IkProps | null): void;

  /**
   * The animation as CSS, as the Motion tab of Inspect gives it with the
   * board selected: `@keyframes` and the `animation` rules of the layers,
   * with the classes the code of Inspect gives them (see
   * `Penpot.generateMarkup`) and keeping the transforms its CSS gives them
   * at rest. The animated component copies in the board play on the clock
   * of their main. A split animation (see `PresetAnimationProps.split`)
   * by characters or words plays each one, with the class
   * `penpot-unit-<n>` the code of Inspect gives it (`penpot-unit-0` the
   * first, inside the class of its text); one by lines, or of a text with
   * a stroke or a shadow (which the code draws as SVG), plays on the
   * whole text. Audio is left out. An empty string when nothing animates.
   */
  toCSS(): string;

  /**
   * The animation as data, version 1 (see `TimelineJson`), as the Motion
   * tab of Inspect gives it as JSON with the board selected: the board,
   * its duration, playback and markers, its animated layers with their
   * keyframes by property and their preset animations, the layers of the
   * animated component copies with the clock of their main, and the audio
   * clips. A plain object: `JSON.stringify(timeline, null, 2)` gives the
   * text of Inspect.
   *
   * @example
   * ```js
   * const motion = board.timeline.toJSON();
   * for (const layer of motion.layers) {
   *   console.log(layer.name, layer.properties.map((p) => p.property));
   * }
   * ```
   */
  toJSON(): TimelineJson;

  /**
   * The animation as React code for motion.dev (`motion/react`), as the
   * Motion tab of Inspect gives it with the board selected: a component
   * for the board, and a `motion.div` for each animated layer, with the
   * class the code of Inspect gives it, its pivot and each value with its
   * keyframes, times and easings, on the clock of the layer (the animated
   * component copies play on the clock of their main). A spring of a
   * value that plays once is a spring of motion.dev; other springs, motion
   * paths and repeating properties are sampled into keyframes. A split
   * animation by characters or words gives the text a `motion.span` for
   * each, with its text and its keyframes (see `toCSS` for the rest);
   * audio is left out. An empty string when nothing animates.
   */
  toReact(): string;

  /**
   * Removes the animation of the board.
   */
  remove(): void;
}

/**
 * The animation of a board as data, version 1 (see `Timeline.toJSON`): the
 * JSON of the Motion tab of Inspect. Times are in milliseconds, positions
 * and sizes in pixels, rotations in degrees; numbers have up to 3
 * decimals. Version 1 only ever gains keys; a change of what a key means
 * makes it version 2.
 */
export interface TimelineJson {
  /**
   * The version of the format: 1.
   */
  version: 1;

  /**
   * What made it: `Penpot`.
   */
  generator: string;

  /**
   * The board: its id, name and size.
   */
  board: { id: string; name: string; width: number; height: number };

  /**
   * How long the timeline plays.
   */
  duration: number;

  /**
   * What the timeline does at its end.
   */
  playback: TimelinePlayback;

  /**
   * The markers of the timeline, by time.
   */
  markers: TimelineJsonMarker[];

  /**
   * The animated layers, in the order of the layers panel: the board,
   * then its layers, top-most first, depth first.
   */
  layers: TimelineJsonLayer[];

  /**
   * The audio clips of the timeline.
   */
  audio: TimelineJsonAudioClip[];
}

/**
 * An animated layer in a `TimelineJson`.
 */
export interface TimelineJsonLayer {
  /**
   * The id of the layer.
   */
  id: string;

  /**
   * The name of the layer.
   */
  name: string;

  /**
   * The type of the layer, as `Shape.type` names it.
   */
  type: string;

  /**
   * The layer as designed: its position relative to the board (its top
   * left corner before it is turned), its size and its rotation.
   */
  rest: {
    x: number;
    y: number;
    width: number;
    height: number;
    rotation: number;
  };

  /**
   * The point the layer turns and scales around, as a share of its box:
   * `{ x: 0.5, y: 0.5 }` is its center.
   */
  origin: { x: number; y: number };

  /**
   * For a text the design animates by units (see
   * `PresetAnimationProps.split`): the kind of units.
   */
  split?: TextSplit;

  /**
   * For a text whose units the code plays one by one (characters or
   * words; see `Timeline.toCSS`): their keyframes. The text's own
   * `properties` then hold what it does as a whole.
   */
  units?: TimelineJsonUnits;

  /**
   * For a layer of a component copy that plays the animation of its main:
   * the copy and the clock of the main.
   */
  component?: TimelineJsonComponent;

  /**
   * The keyframes of the layer, by property, in the order of the timeline
   * dock. The preset animations of the layer are among them, as keyframes.
   */
  properties: TimelineJsonProperty[];

  /**
   * The preset animations of the layer, those the uses of animation styles
   * play included.
   */
  presets: TimelineJsonPreset[];
}

/**
 * The keyframes of one property of a layer in a `TimelineJson`.
 */
export interface TimelineJsonProperty {
  /**
   * The property, see `AnimatableProperty` for its values.
   */
  property: AnimatableProperty;

  /**
   * For fills, strokes and shadows: which one of the layer.
   */
  index?: number;

  /**
   * Whether the keyframes repeat over the timeline: they are one round,
   * played again and again up to its end.
   */
  loop?: boolean;

  /**
   * The keyframes, by time.
   */
  keyframes: TimelineJsonKeyframe[];
}

/**
 * The units of a text the code plays one by one, in a `TimelineJsonLayer`.
 */
export interface TimelineJsonUnits {
  /**
   * The kind of units: `char` or `word`.
   */
  kind: TextSplit;

  /**
   * How many units the text has, as the text splits them (spaces are
   * none).
   */
  count: number;

  /**
   * The keyframes of each unit, by its index from 0, by property: `x` and
   * `y` offsets in pixels from where the unit rests, `rotation` in
   * degrees, `scaleX`, `scaleY` and `opacity` factors and `blur` in
   * pixels.
   */
  keyframes: { index: number; properties: TimelineJsonProperty[] }[];
}

/**
 * A keyframe in a `TimelineJsonProperty`.
 */
export interface TimelineJsonKeyframe {
  /**
   * When it is.
   */
  time: number;

  /**
   * The value of the property then, see `AnimatableProperty`.
   */
  value: number | string;

  /**
   * How the value goes on from here to the next keyframe; the last
   * keyframe has none.
   */
  easing?: TimelineJsonEasing;

  /**
   * The design token that sets its easing, by name, see
   * `Keyframe.appliedTokens`.
   */
  tokens?: KeyframeTokens;

  /**
   * For `x` and `y` on a motion path: where the curve comes into the
   * keyframe from, as an offset from its value, like the handle of a
   * bezier node.
   */
  pathIn?: number;

  /**
   * For `x` and `y` on a motion path: where the curve leaves the keyframe
   * towards, as an offset from its value.
   */
  pathOut?: number;
}

/**
 * How a value goes from a keyframe to the next one, or how a preset
 * animation plays, in a `TimelineJson`:
 * - `{ type: 'bezier', curve: [x1, y1, x2, y2] }`, like the CSS
 *   `cubic-bezier()`, with the `name` of the CSS easing it is, if any
 *   (`ease` is `[0.25, 0.1, 0.25, 1]`).
 * - `{ type: 'spring', stiffness, damping, mass }`: a spring that settles
 *   at the next keyframe.
 * - `{ type: 'hold' }`: the value stays until the next keyframe.
 */
export type TimelineJsonEasing =
  | {
      type: 'bezier';
      curve: [number, number, number, number];
      name?: 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out';
    }
  | { type: 'spring'; stiffness: number; damping: number; mass: number }
  | { type: 'hold' };

/**
 * A preset animation of a layer in a `TimelineJson`, see
 * `PresetAnimationProps`.
 */
export interface TimelineJsonPreset {
  /**
   * The effect.
   */
  type: PresetAnimationType;

  /**
   * Whether the layer enters or exits.
   */
  direction: 'in' | 'out';

  /**
   * When it starts.
   */
  start: number;

  /**
   * How long it plays.
   */
  duration: number;

  /**
   * How it plays, when it is set.
   */
  easing?: TimelineJsonEasing;

  /**
   * How much, when it is set, see `PresetAnimationType`.
   */
  amount?: number;

  /**
   * For `move`: where the layer comes from or goes to, in pixels from
   * where it is.
   */
  offsetX?: number;

  /**
   * For `move`: as `offsetX`, vertically.
   */
  offsetY?: number;

  /**
   * The name of the animation style it comes from, if any.
   */
  style?: string;

  /**
   * The design tokens that set its settings, if any.
   */
  tokens?: AnimationStyleEntryTokens;
}

/**
 * The component copy a layer of a `TimelineJson` belongs to, which plays
 * the animation of its main on its own clock (see `Board.animatedCopies`).
 * The animation itself is in the `TimelineJson` of the main's board.
 */
export interface TimelineJsonComponent {
  /**
   * The id of the copy: the top layer of the component copy.
   */
  copy: string;

  /**
   * The name of the copy.
   */
  name: string;

  /**
   * How long the timeline of the main plays.
   */
  duration: number;

  /**
   * What the timeline of the main does at its end.
   */
  playback: TimelinePlayback;
}

/**
 * A marker in a `TimelineJson`, see `Marker`.
 */
export interface TimelineJsonMarker {
  /**
   * The name of the marker.
   */
  name: string;

  /**
   * When it is.
   */
  time: number;
}

/**
 * An audio clip in a `TimelineJson`, see `AudioClip`.
 */
export interface TimelineJsonAudioClip {
  /**
   * The name of its file.
   */
  name: string;

  /**
   * When it starts playing on the timeline.
   */
  start: number;

  /**
   * How long it plays on the timeline.
   */
  duration: number;

  /**
   * Where in its file it starts playing.
   */
  offset: number;

  /**
   * How fast it plays its file, 1 being as recorded.
   */
  speed: number;

  /**
   * How loud it plays, see `AudioClip.volume`.
   */
  volume: number;

  /**
   * How long it fades in at its start.
   */
  fadeIn: number;

  /**
   * How long it fades out at its end.
   */
  fadeOut: number;

  /**
   * Whether it is silent.
   */
  muted: boolean;
}

/**
 * Where a follow counts from, see `Follow.from`: the first frame of the
 * leader (`'start'`) or where it rests (`'rest'`).
 */
export type FollowFrom = 'start' | 'rest';

/**
 * How a layer follows another layer of its board, see `Timeline.setFollow`.
 */
export interface Follow {
  /**
   * The layer it follows.
   */
  readonly leader: Shape;

  /**
   * What it takes of the leader: what the leader does from its first
   * frame on (`'start'`), or all that moves it from where it rests
   * (`'rest'`), as a layer of a group does.
   */
  readonly from: FollowFrom;

  /**
   * Whether it moves as the pivot of the leader moves.
   */
  readonly position: boolean;

  /**
   * Whether it turns with the leader, around the leader's pivot.
   */
  readonly rotation: boolean;

  /**
   * Whether it scales with the leader, around the leader's pivot.
   */
  readonly scale: boolean;

  /**
   * Whether it tilts as the leader does: the change of the leader's 3D
   * rotation (`rotateX`, `rotateY`), around its own pivot.
   */
  readonly tilt: boolean;

  /**
   * Whether its opacity is multiplied by the leader's.
   */
  readonly opacity: boolean;

  /**
   * How much later it does what the leader does, in milliseconds.
   */
  readonly delay: number;
}

/**
 * The leader of a layer and what the layer takes of it, see
 * `Timeline.setFollow`.
 */
export interface FollowProps {
  /**
   * The layer to follow: another layer of the board.
   */
  leader: Shape;

  /**
   * Take what the leader does from its first frame on (`'start'`), or
   * all that moves it from where it rests, as a layer of a group does
   * (`'rest'`, for a leader that pops or slides in). `'start'` by default.
   */
  from?: FollowFrom;

  /**
   * Move as the pivot of the leader moves. `true` by default.
   */
  position?: boolean;

  /**
   * Turn with the leader, around its pivot. `true` by default.
   */
  rotation?: boolean;

  /**
   * Scale with the leader, around its pivot. `true` by default.
   */
  scale?: boolean;

  /**
   * Tilt as the leader does (its 3D rotation, `rotateX` and `rotateY`),
   * around its own pivot. `true` by default.
   */
  tilt?: boolean;

  /**
   * Multiply its opacity by the leader's. `false` by default.
   */
  opacity?: boolean;

  /**
   * Do what the leader does this many milliseconds later. 0 by default.
   */
  delay?: number;
}

/**
 * The properties a link drives and reads (see `Timeline.link`): those of
 * one number each.
 */
export type LinkableProperty =
  | 'x'
  | 'y'
  | 'width'
  | 'height'
  | 'rotation'
  | 'scaleX'
  | 'scaleY'
  | 'opacity'
  | 'rotateX'
  | 'rotateY'
  | 'translateZ'
  | 'blur';

/**
 * How a property of a layer plays a property of another layer of the
 * board, its source, as the Links section of the Motion tab sets it (see
 * `Timeline.link`).
 */
export interface Link {
  /**
   * The property it drives.
   */
  readonly property: LinkableProperty;

  /**
   * The layer whose property it plays.
   */
  readonly source: Shape;

  /**
   * The property of the source it plays.
   */
  readonly sourceProperty: LinkableProperty;

  /**
   * How much it changes for each unit the source changes by.
   */
  readonly factor: number;

  /**
   * Added to it, in its own unit.
   */
  readonly offset: number;

  /**
   * How many milliseconds after the source it plays.
   */
  readonly delay: number;

  /**
   * With a range: the values of the source, from and to, that take the
   * property through `output` (factor and offset left aside).
   */
  readonly input?: [number, number];

  /**
   * With a range: the values of the property, from and to, as the source
   * goes through `input`.
   */
  readonly output?: [number, number];

  /**
   * With a range: whether the property stays within `output` when the
   * source leaves `input`.
   */
  readonly clamp?: boolean;

  /**
   * With a range: the curve it follows from one end to the other.
   */
  readonly easing?: LinkEasing;
}

/**
 * How the trim of a group draws the layers in it, see
 * `Timeline.setTrimMode`.
 */
export type TrimMode = 'together' | 'in-turn';

/**
 * The curve a link with a range follows, see `LinkProps.easing`.
 */
export type LinkEasing = 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out';

/**
 * A link to set with `Timeline.link`.
 */
export interface LinkProps {
  /**
   * The property to drive.
   */
  property: LinkableProperty;

  /**
   * The layer to read: a layer of the board, the shape itself too.
   */
  source: Shape;

  /**
   * The property of the source to read; `property` by default.
   */
  sourceProperty?: LinkableProperty;

  /**
   * How much it changes for each unit the source changes by. 1 by
   * default.
   */
  factor?: number;

  /**
   * Added to it, in its own unit. 0 by default.
   */
  offset?: number;

  /**
   * Play this many milliseconds after the source. 0 by default.
   */
  delay?: number;

  /**
   * A range of the source, `[from, to]`: going through it, the source
   * takes the property through `output`, as an AE `linear()` or `ease()`
   * expression does. Give both or neither; factor and offset do not apply
   * then.
   *
   * A slider from x 0 to 200 turns a needle from 0 to 270 degrees:
   *
   * @example
   * ```js
   * timeline.link(needle, { property: "rotation", source: slider, sourceProperty: "x",
   *                         input: [0, 200], output: [0, 270], easing: "ease-in-out" });
   * ```
   */
  input?: [number, number];

  /**
   * The range of the property, `[from, to]`, the source takes it through
   * as it goes through `input`.
   */
  output?: [number, number];

  /**
   * Keep the property within `output` when the source leaves `input`.
   * True by default.
   */
  clamp?: boolean;

  /**
   * The curve from one end of the range to the other. `'linear'` by
   * default.
   */
  easing?: LinkEasing;
}

/**
 * The side the elbow of a chain bends to as it reaches, see
 * `Timeline.setIk`: clockwise (`'cw'`) or counter-clockwise (`'ccw'`).
 */
export type IkBend = 'cw' | 'ccw';

/**
 * How the end of a chain turns as it reaches, see `Timeline.setIk`:
 * along with the chain (`'chain'`), keeping its own angle (`'keep'`), or
 * taking the turn of the goal (`'goal'`).
 */
export type IkEnd = 'chain' | 'keep' | 'goal';

/**
 * The goal the end of a chain of two follows reaches, see
 * `Timeline.setIk`.
 */
export interface Ik {
  /**
   * The layer it reaches.
   */
  readonly goal: Shape;

  /**
   * The side its elbow bends to.
   */
  readonly bend: IkBend;

  /**
   * How the end of the chain turns.
   */
  readonly end: IkEnd;
}

/**
 * The goal of the end of a chain of two follows, see `Timeline.setIk`.
 */
export interface IkProps {
  /**
   * The layer to reach: another layer of the board, out of the chain.
   */
  goal: Shape;

  /**
   * The side the elbow bends to. By default the side the chain is drawn
   * bent to at its first frame, or the one it bent to before.
   */
  bend?: IkBend;

  /**
   * How the end of the chain turns. `'chain'` by default, or as before.
   */
  end?: IkEnd;
}

/**
 * What `Timeline.copyAnimation` copies and how it pastes it.
 */
export interface CopyAnimationOptions {
  /**
   * Copy only the keyframes of these properties. All of them by default.
   */
  properties?: AnimatableProperty[];

  /**
   * Whether the preset animations are copied too. `true` by default.
   */
  presets?: boolean;

  /**
   * `relative` (the default): the targets move, turn and resize by as much
   * as the shape does, from where they rest. `exact`: they take the values
   * of the shape.
   */
  values?: 'relative' | 'exact';

  /**
   * When the copy starts on the targets, in milliseconds: its first
   * keyframe or preset animation lands there. By default it keeps the
   * timing of the shape.
   */
  start?: number;

  /**
   * How much later each target starts than the one before it in `targets`,
   * in milliseconds. 0 by default.
   */
  stagger?: number;

  /**
   * Whether the copy takes the place of the keyframes the targets have of
   * the same properties, and of their preset animations when it has some
   * (`true`, the default), or is added to them (`false`). Locked
   * properties keep their keyframes.
   */
  replace?: boolean;
}

/**
 * What `Timeline.copyAnimation` did.
 */
export interface CopyAnimationResult {
  /**
   * How many targets got the animation.
   */
  readonly targets: number;

  /**
   * How many keyframes were left out because the target has no matching
   * fill, stroke, shadow, blur or corner radius.
   */
  readonly skipped: number;

  /**
   * How many targets kept their own pivot because they already turn, scale
   * or resize: the copy turns, scales or resizes them around it.
   */
  readonly pivotKept: number;
}

/**
 * How a board is exported as an animation, see `Timeline.exportSettings`.
 * Reading it gives all of them; setting it changes the ones given.
 */
export interface TimelineExportSettings {
  /**
   * The file: a video (`mp4`, `webm`), an animated image (`gif`, `avif`),
   * an animated `svg`, or a Lottie animation as JSON (`lottie`) or as a
   * dotLottie archive with its images as files (`dotlottie`). `mp4` by
   * default.
   */
  format?: TimelineExportFormat;

  /**
   * How good a video or an animated image looks, and so how big its file
   * is. `high` by default.
   */
  quality?: 'low' | 'medium' | 'high';

  /**
   * Frames per second of a video or an animated image, 1 to 120. 30 by
   * default.
   */
  fps?: number;

  /**
   * How big it is: a factor of the board up to 10 (`0.5`, `1`, `2`,
   * `3.5`) or a width or height in pixels up to 8192 (`w1920`, `h720`).
   * `1` by default. It takes what the export panel takes (`3,5`, `3.5x`,
   * `512w`, `720h`) and keeps it as above.
   */
  size?: TimelineExportSize;

  /**
   * Whether a GIF or AVIF plays over and over. `true` by default.
   */
  loop?: boolean;

  /**
   * Whether an MP4 or WebM video has the sound of the audio clips of the
   * timeline. `true` by default; the other formats have no sound.
   */
  audio?: boolean;

  /**
   * Whether an SVG gives each layer, and the board, its name as its
   * `id` attribute (made a valid XML name, the second layer of a name
   * `<name>_2` and so on), as the "Include id attribute" option of Figma.
   * `false` by default.
   */
  includeIds?: boolean;

  /**
   * Whether an SVG starts with a bounding box: an empty rectangle over
   * the board, as the "Include bounding box" option of Figma. `false`
   * by default.
   */
  includeBoundingBox?: boolean;

  /**
   * Whether an SVG has its texts as the outlines of their glyphs instead
   * of text with its fonts, as the "Outline text" option of Figma: a
   * smaller file that looks the same everywhere, whose text cannot be
   * selected. The characters, words or lines of a text animated by them
   * are outlines anyway. `false` by default.
   */
  outlineText?: boolean;
}

/**
 * The file formats of an animation export, see `TimelineExportSettings`.
 */
export type TimelineExportFormat =
  'mp4' | 'webm' | 'gif' | 'avif' | 'svg' | 'lottie' | 'dotlottie';

/**
 * The size of an animation export, see `TimelineExportSettings`: a factor
 * of the board (`'2'`, `'3.5'`), a width (`'w1920'`) or a height (`'h720'`)
 * in pixels.
 */
export type TimelineExportSize = `${number}` | `w${number}` | `h${number}`;

/**
 * A component copy in a board that plays the animation of its main, see
 * `Board.animatedCopies`.
 */
export interface AnimatedCopy {
  /**
   * The copy: the top layer of the component copy.
   */
  readonly copy: Shape;

  /**
   * How long a round of its animation takes, in milliseconds (a ping-pong
   * main plays there and back in a round).
   */
  readonly duration: number;

  /**
   * Whether it plays once or over and over, on its own clock.
   */
  readonly playback: 'once' | 'loop';
}

/**
 * A named moment of a timeline, see `Timeline.markers`.
 */
export interface Marker {
  /**
   * The id of the marker.
   */
  readonly id: string;

  /**
   * The name of the marker.
   */
  name: string;

  /**
   * When it is, in milliseconds. The timeline grows to hold it.
   */
  time: number;

  /**
   * Removes the marker.
   */
  remove(): void;
}

/**
 * The time and name of a new marker, see `Timeline.addMarker`.
 */
export interface MarkerProps {
  /**
   * When it is, in milliseconds from the start.
   */
  time: number;

  /**
   * Its name, `Marker N` by default.
   */
  name?: string;
}

/**
 * A sound file a timeline plays, see `Timeline.audioClips`: a part of the
 * file (from `offset`) from `start` on the timeline for `duration`, at a
 * `speed`, on a lane of the timeline dock. Times are in milliseconds:
 * `start`, `duration` and the fades of the timeline, `offset` and
 * `sourceDuration` of the file. A locked clip takes no change but its
 * unlocking and a new `name`.
 */
export interface AudioClip {
  /**
   * The id of the clip.
   */
  readonly id: string;

  /**
   * The name of its file, shown on the clip and on its lane. Setting it
   * renames the file, as a double click on its lane does: all the clips
   * that play it take the name (trimmed), locked ones too. A blank name
   * is not valid.
   *
   * @example
   * ```js
   * const [clip] = board.timeline.audioClips;
   * clip.name = 'Voice-over';
   * ```
   */
  name: string;

  /**
   * How long its whole file plays.
   */
  readonly sourceDuration: number;

  /**
   * When it starts playing on the timeline, in milliseconds. Before 0 it
   * sounds from the start of the timeline on, as far into its file, as in
   * the timeline dock.
   */
  start: number;

  /**
   * Where in its file it starts playing. It plays less when the file ends
   * before the clip does.
   */
  offset: number;

  /**
   * How long it plays on the timeline, at least 10 milliseconds and at
   * most to the end of its file at its speed.
   */
  duration: number;

  /**
   * How fast it plays its file, from 0.25 to 4 (1 by default): at 2 it
   * plays the same part of the file in half the time and an octave
   * higher, as a tape does. Setting it keeps that part, so `duration`
   * changes with it.
   */
  speed: number;

  /**
   * The row of the timeline dock it is on, from 0, named after its file. A
   * lane plays a single file: its clips are parts of it, which may
   * overlap, and setting a lane that holds another file is refused.
   */
  lane: number;

  /**
   * Its volume, from 0 (silent) to 4: 1 plays its file as loud as it
   * is, above it louder (4 is +12 dB; loud files may then clip).
   */
  volume: number;

  /**
   * Whether it is silent.
   */
  muted: boolean;

  /**
   * How long it fades in from silence at its start, at most half of it.
   */
  fadeIn: number;

  /**
   * How long it fades out to silence at its end, at most half of it.
   */
  fadeOut: number;

  /**
   * Whether it takes no changes, but its unlocking.
   */
  locked: boolean;

  /**
   * Cuts the clip at `time` of the timeline into two clips of the same
   * file: this one keeps the part before and its fade in, and the new one,
   * which it returns, the part after and its fade out. Null when `time` is
   * not inside the clip, or a part would last less than 10 milliseconds.
   * @param time A time of the timeline, inside the clip.
   */
  split(time: number): AudioClip | null;

  /**
   * Removes the clip.
   */
  remove(): void;
}

/**
 * Where a new audio clip plays from, see `Timeline.addAudioClip`.
 */
export interface AudioClipProps {
  /**
   * When it starts playing on the timeline, in milliseconds (0 by
   * default), before 0 too (see `AudioClip.start`).
   */
  start?: number;

  /**
   * The lane of the timeline dock it goes on, from 0: one without clips,
   * since a lane plays a single file. By default the first lane without
   * clips.
   */
  lane?: number;
}

/**
 * The settings of a new timeline, see `Board.addTimeline`.
 */
export interface TimelineOptions {
  /**
   * The name of the animation.
   */
  name?: string;
  /**
   * How long it plays, in milliseconds (1000 by default).
   */
  duration?: number;
  /**
   * What playback does at the end (`once` by default).
   */
  playback?: TimelinePlayback;
  /**
   * Whether the prototype plays it when it shows the board (`true` by
   * default), see `Timeline.autoplay`.
   */
  autoplay?: boolean;
}

/**
 * What playback of a timeline does at its end: stop there (`once`), start
 * over (`loop`) or play backwards and on (`ping-pong`).
 */
export type TimelinePlayback = 'once' | 'loop' | 'ping-pong';

/**
 * A property a keyframe can animate, and what its value is:
 * - `x`, `y`: the position of the layer itself (its top left corner before
 *   it is turned) relative to the board, in pixels; the board's own is
 *   relative to the canvas.
 * - `width`, `height`, `strokeWidth`, `blur`, `backgroundBlur`,
 *   `shadowOffsetX`, `shadowOffsetY`, `shadowBlur`, `shadowSpread` and the
 *   `borderRadius…` corners: pixels.
 * - `rotation`, `rotateX`, `rotateY`: degrees.
 * - `translateZ`: how far the layer comes towards the viewer, in pixels.
 * - `perspective`: the distance of the viewer of the tilted layers of a
 *   board, in pixels; `perspectiveX`, `perspectiveY`: their vanishing
 *   point, a fraction of the board (0.5 is its middle). Only boards take
 *   them (see `Board.perspective`).
 * - `lightAngle`, `lightHeight`: where the light of a board comes from,
 *   degrees round the view (clockwise from its top) and above it;
 *   `lightAmbient`: the share of it from all round (0 to 1). Only boards
 *   with a light take them (see `Board.light`).
 * - `scaleX`, `scaleY`: a factor, 1 is the layer's own size.
 * - `opacity`, `fillOpacity`, `strokeOpacity`, `shadowOpacity`: 0 to 1.
 * - `fillColor`, `strokeColor`, `shadowColor`: a hexadecimal color like
 *   `#ff6600`.
 * - `trimStart`, `trimEnd`, `trimOffset`: a fraction of the outline (0 to
 *   1) the strokes are drawn along, to draw a line on. On a group they
 *   trim the layers in it (see `Timeline.setTrimMode`).
 * - `path`: the outline of a path layer, an SVG path like its `d`, in
 *   page coordinates where the path rests. The outline morphs from one
 *   keyframe to the next, points added where two have not as many. Only
 *   path layers take it (turn a rectangle or an ellipse into a path
 *   first); it takes no motion path (`pathIn`, `pathOut`) and no animation
 *   style holds it. Moving, turning or resizing the path takes its
 *   outlines along; setting its `d` changes the box they are kept in.
 *
 * The fill, stroke and shadow properties take the `index` of the fill,
 * stroke or shadow of the layer.
 */
export type AnimatableProperty =
  | 'x'
  | 'y'
  | 'width'
  | 'height'
  | 'rotation'
  | 'rotateX'
  | 'rotateY'
  | 'translateZ'
  | 'perspective'
  | 'perspectiveX'
  | 'perspectiveY'
  | 'lightAngle'
  | 'lightHeight'
  | 'lightAmbient'
  | 'scaleX'
  | 'scaleY'
  | 'opacity'
  | 'borderRadiusTopLeft'
  | 'borderRadiusTopRight'
  | 'borderRadiusBottomRight'
  | 'borderRadiusBottomLeft'
  | 'fillColor'
  | 'fillOpacity'
  | 'strokeColor'
  | 'strokeOpacity'
  | 'strokeWidth'
  | 'shadowOffsetX'
  | 'shadowOffsetY'
  | 'shadowBlur'
  | 'shadowSpread'
  | 'shadowColor'
  | 'shadowOpacity'
  | 'blur'
  | 'backgroundBlur'
  | 'trimStart'
  | 'trimEnd'
  | 'trimOffset'
  | 'path';

/**
 * How a value goes from a keyframe to the next one, or how a preset
 * animation plays:
 * - A preset curve: `linear`, `ease`, `ease-in`, `ease-out` or
 *   `ease-in-out`, like in CSS.
 * - `hold`: the value stays until the next keyframe (keyframes only).
 * - A cubic bezier curve, like the CSS `cubic-bezier()`:
 *   `{ type: 'bezier', curve: [x1, y1, x2, y2] }`.
 * - A spring that settles at the next keyframe:
 *   `{ type: 'spring', stiffness: 170, damping: 26, mass: 1 }`.
 */
export type MotionEasing =
  | 'linear'
  | 'ease'
  | 'ease-in'
  | 'ease-out'
  | 'ease-in-out'
  | 'hold'
  | { type: 'bezier'; curve: [number, number, number, number] }
  | { type: 'spring'; stiffness: number; damping: number; mass: number };

/**
 * A keyframe to add, see `Timeline.addKeyframe`.
 */
export interface KeyframeProps {
  /**
   * The property it sets, see `AnimatableProperty` for its values.
   */
  property: AnimatableProperty;
  /**
   * When, in milliseconds from the start of the animation. Before 0 it
   * does not play, but sets where the animation is at its start, as in
   * the timeline dock.
   */
  time: number;
  /**
   * The value of the property then: a number, a hexadecimal color for the
   * colors, or an SVG path for `path` (see `AnimatableProperty`).
   */
  value: number | string;
  /**
   * How the value goes on from here to the next keyframe (`ease` by
   * default).
   */
  easing?: MotionEasing;
  /**
   * For fills, strokes and shadows: which one of the layer (0 by default).
   */
  index?: number;
  /**
   * The design token that sets its easing, see `Keyframe.appliedTokens`.
   */
  appliedTokens?: KeyframeTokens;
}

/**
 * The design token that sets the easing of a keyframe, by its name: a
 * `cubicBezier` or a `spring` token (see `Keyframe.appliedTokens`).
 */
export interface KeyframeTokens {
  easing?: string;
}

/**
 * The value of a property of a layer at a time, in a `Timeline`.
 */
export interface Keyframe {
  /**
   * The id of the keyframe.
   */
  readonly id: string;
  /**
   * The board or layer it animates.
   */
  readonly shape: Shape;
  /**
   * The property it sets.
   */
  readonly property: AnimatableProperty;
  /**
   * For fills, strokes and shadows: which one of the layer.
   */
  readonly index?: number;
  /**
   * When, in milliseconds from the start of the animation, before 0 too
   * (see `KeyframeProps.time`).
   */
  time: number;
  /**
   * The value of the property then, see `AnimatableProperty`.
   */
  value: number | string;
  /**
   * For a `path` keyframe, whether its first point and the way it goes
   * round were set by hand. Penpot pairs the points of consecutive path
   * keyframes of other structures on its own, so that each point takes
   * the shortest way; a fixed keyframe instead turns as the keyframe
   * before it does, so its first point goes to that one's first point
   * (as Lottie and After Effects pair them). Set its `value` to start at
   * the point wanted, then set `fixedStart` to `true`; `false` matches it
   * on its own again. Other keyframes have none: setting it throws.
   *
   * @example
   * ```js
   * keyframe.value = 'M100,0 L200,100 L100,200 L0,100 Z';
   * keyframe.fixedStart = true;
   * ```
   */
  fixedStart: boolean;
  /**
   * How the value goes on from here to the next keyframe. Setting it
   * unbinds the token of `appliedTokens`.
   */
  easing: MotionEasing;
  /**
   * The design token that sets its easing, by name, or undefined. The
   * file resolves it in its active sets and writes its value into
   * `easing`; when the token changes, the keyframe follows. Setting it
   * binds a token, `undefined` unbinds it and keeps the easing.
   *
   * @example
   * ```js
   * keyframe.appliedTokens = { easing: 'ease.bounce' };
   * ```
   */
  appliedTokens: KeyframeTokens | undefined;
  /**
   * Removes the keyframe.
   */
  remove(): void;
}

/**
 * The effect of a preset animation:
 * - `fade`: the opacity, from `amount` (0 by default) to the layer's own.
 * - `move`: the position, from `offsetX` and `offsetY` pixels away.
 * - `scale`: the size, from `amount` times it (0.5 by default).
 * - `rotate`: the rotation, from `amount` degrees turned (-90 by default).
 * - `flip-x` and `flip-y`: the 3D rotation about the layer's own X or Y
 *   axis (`rotateX`, `rotateY`), from `amount` degrees turned (90 by
 *   default), around the pivot of its track: `flip-y` turns a card over.
 * - `blur`: a layer blur, from a radius of `amount` pixels (8 by default) to
 *   the layer's own; a layer without a blur gets one while it plays.
 */
export type PresetAnimationType =
  | 'fade'
  | 'move'
  | 'scale'
  | 'rotate'
  | 'flip-x'
  | 'flip-y'
  | 'blur';

/**
 * A preset animation to add, see `Timeline.addAnimation`.
 */
export interface PresetAnimationProps {
  /**
   * The effect.
   */
  type: PresetAnimationType;
  /**
   * `in` plays from the effect to the layer as it is (an entrance), `out`
   * from the layer to the effect (an exit). `in` by default.
   */
  direction?: 'in' | 'out';
  /**
   * When it starts, in milliseconds (0 by default). On a timeline before
   * 0 too, where it does not play but sets where the animation is at its
   * start; in a style from the start of the style on.
   */
  start?: number;
  /**
   * How long it plays, in milliseconds.
   */
  duration?: number;
  /**
   * How it plays (`ease-out` for an entrance and `ease-in` for an exit by
   * default). It cannot hold.
   */
  easing?: MotionEasing;
  /**
   * For `fade`, `scale`, `rotate` and `blur`: where the effect starts or ends, see
   * `PresetAnimationType`.
   */
  amount?: number;
  /**
   * For `move`: the horizontal offset, in pixels.
   */
  offsetX?: number;
  /**
   * For `move`: the vertical offset, in pixels.
   */
  offsetY?: number;
  /**
   * Text layers only: plays the animation on each character, word or
   * line of the text, one after the other, within its span. A text plays
   * one kind of units: all its split animations split alike. The text
   * stays editable, and typing gives more units. The video, SVG, Lottie
   * and dotLottie exports play the units (an SVG or Lottie as a picture
   * of each, but of a text that underlines or strikes some of its text,
   * which plays whole); the code (`Timeline.toCSS`, `toReact`, `toJSON`)
   * plays characters and words, a split by lines on the whole text. A
   * split `flip-x` or `flip-y` turns each unit over in 3D, seen from the
   * perspective of the board in front of its middle (the SVG and Lottie
   * exports show it in the plane).
   */
  split?: TextSplit;
  /**
   * With `split`: how far apart the units start, from 0 (all together,
   * each over the whole span) to 1 (one after the other); 0.5 by default.
   * The units always play within the span.
   */
  spread?: number;
  /**
   * With `split`: the order the units start in, `forward` by default.
   */
  order?: TextSplitOrder;
  /**
   * With `split` and a `random` order: the seed of its shuffle, so the
   * order stays the same. Set when not given.
   */
  seed?: number;
  /**
   * The design tokens that set settings of the animation, by setting,
   * see `PresetAnimation.appliedTokens`. The values of the tokens win
   * over the settings given.
   */
  appliedTokens?: AnimationStyleEntryTokens;
}

/**
 * What a split animation plays one by one: the characters (not the
 * spaces), the words (with the punctuation next to them) or the lines of
 * a text.
 */
export type TextSplit = 'char' | 'word' | 'line';

/**
 * The order the units of a split animation start in: from the first, from
 * the last, from the middle, from both ends, or shuffled (see
 * `PresetAnimationProps.seed`).
 */
export type TextSplitOrder =
  'forward' | 'backward' | 'center' | 'edges' | 'random';

/**
 * An effect over a span of time of a layer, in a `Timeline`. Within its
 * span it takes over the keyframes of the properties it animates.
 */
export interface PresetAnimation {
  /**
   * The id of the animation.
   */
  readonly id: string;
  /**
   * The board or layer it animates.
   */
  readonly shape: Shape;
  /**
   * The effect.
   */
  readonly type: PresetAnimationType;
  /**
   * `in` (an entrance) or `out` (an exit).
   */
  direction: 'in' | 'out';
  /**
   * When it starts, in milliseconds, before 0 too (see
   * `PresetAnimationProps.start`).
   */
  start: number;
  /**
   * How long it plays, in milliseconds.
   */
  duration: number;
  /**
   * How it plays.
   */
  easing: MotionEasing;
  /**
   * For `fade`, `scale`, `rotate` and `blur`, see `PresetAnimationType`.
   */
  amount?: number;
  /**
   * For `move`: the horizontal offset, in pixels.
   */
  offsetX?: number;
  /**
   * For `move`: the vertical offset, in pixels.
   */
  offsetY?: number;
  /**
   * The units of a text it plays one by one, see
   * `PresetAnimationProps.split`; none for the whole layer. Setting
   * another kind than the other split animations of the text have is
   * refused.
   */
  split?: TextSplit;
  /**
   * With `split`: how far apart the units start, from 0 to 1.
   */
  spread?: number;
  /**
   * With `split`: the order the units start in.
   */
  order?: TextSplitOrder;
  /**
   * With `split` and a `random` order: the seed of its shuffle. A new one
   * shuffles the units again.
   */
  seed?: number;
  /**
   * The design tokens that set settings of the animation, by setting, or
   * undefined (see `AnimationStyleEntryTokens` for the settings and the
   * types of their tokens). The file resolves them in its active sets
   * and writes their values into the settings; when a token changes, the
   * animation follows. Setting a setting by hand unbinds its token;
   * setting `appliedTokens` binds tokens, `undefined` unbinds them all.
   *
   * @example
   * ```js
   * animation.appliedTokens = { duration: 'motion.fast', easing: 'ease.spring' };
   * ```
   */
  appliedTokens: AnimationStyleEntryTokens | undefined;
  /**
   * Removes the animation.
   */
  remove(): void;
}

/**
 * Animation styles, several preset animations at once:
 * - `slide-up`, `slide-down`, `slide-left`, `slide-right`: a fade and a
 *   move of 40 pixels.
 * - `pop`: a fade and a scale from half the size that overshoots.
 * - `zoom`: a fade and a scale from 1.5 times the size.
 * - `spin`: a fade, a scale and half a turn.
 *
 * And, for text layers only (split animations, see
 * `PresetAnimationProps.split`):
 * - `typewriter`: the characters fade in one after the other.
 * - `rise-words`: the words fade in and rise.
 * - `cascade-lines`: the lines fade in and rise.
 * - `pop-letters`: the characters fade in and pop from a small size.
 * - `flip-letters`: the characters fade in and turn over about their
 *   horizontal middle, in 3D (Penpot Motion).
 */
export type AnimationStyle =
  | 'slide-up'
  | 'slide-down'
  | 'slide-left'
  | 'slide-right'
  | 'pop'
  | 'zoom'
  | 'spin'
  | 'typewriter'
  | 'rise-words'
  | 'cascade-lines'
  | 'pop-letters'
  | 'flip-letters';

/**
 * An entry of an animation style (`LibraryAnimationStyle`), its `start`
 * counted from the start of the style: a preset animation
 * (`PresetStyleEntry`) or the keyframes of one property
 * (`KeyframesStyleEntry`, `type: 'keyframes'`).
 */
export type AnimationStyleEntry = PresetStyleEntry | KeyframesStyleEntry;

/**
 * A preset animation of an animation style (`LibraryAnimationStyle`), its
 * `start` counted from the start of the style. A split entry plays the
 * units of a text layer, and the whole layer on other layers. Design
 * tokens can set some of its settings, see `appliedTokens`.
 */
export type PresetStyleEntry = PresetAnimationProps & {
  /**
   * The design tokens that set settings of the entry, by setting. The file
   * of the style resolves them in its active sets and writes their values
   * into the entry (when they change, the style and its uses follow);
   * the uses of the style in other files keep the names and the values.
   * Setting `LibraryAnimationStyle.entries` with them binds them.
   *
   * @example
   * ```js
   * style.entries = [
   *   { type: 'fade', start: 0, duration: 150, appliedTokens: { duration: 'motion.fast', easing: 'ease.out' } },
   * ];
   * ```
   */
  appliedTokens?: AnimationStyleEntryTokens;
};

/**
 * The keyframes of one property of an animation style
 * (`LibraryAnimationStyle`): from `start`, over `duration`, they play in
 * the row of the property on every layer that uses the style, but where
 * the layer has keyframes of its own in that row: from the first to the
 * last of them its own keyframes play; preset animations of the same
 * property win over both. Their values are relative to the layer
 * that plays them, so a style moves, turns and resizes each layer from
 * where it rests: `x` and `y` are offsets in pixels from its rest
 * position and `rotation` in degrees from its own, `width` and `height`
 * factors of its own size (1 is its size), the other properties as they
 * are. A layer with a row of keyframes saved as a style, or a style
 * updated from a layer (`LibraryAnimationStyle.updateFromShape`), gives
 * them; a style holds at most 256 keyframes in all.
 *
 * @example
 * ```js
 * const wobble = penpot.library.local.createAnimationStyle([
 *   { type: 'keyframes', property: 'rotation', start: 0, duration: 600, keyframes: [
 *     { time: 0, value: 0, easing: 'ease-in-out' }, { time: 200, value: 8, easing: 'ease-in-out' },
 *     { time: 400, value: -8, easing: 'ease-in-out' }, { time: 600, value: 0 },
 *   ] },
 * ]);
 * ```
 */
export interface KeyframesStyleEntry {
  /**
   * The kind of entry.
   */
  type: 'keyframes';
  /**
   * The property its keyframes set, see `AnimatableProperty`.
   */
  property: AnimatableProperty;
  /**
   * For fills, strokes and shadows: which one of the layer (0 by default).
   */
  index?: number;
  /**
   * When it starts, in milliseconds from the start of the style (0 by
   * default).
   */
  start?: number;
  /**
   * How long it plays, in milliseconds: its keyframes are within it (as
   * long as they last by default). Stretching a use of the style
   * stretches them too.
   */
  duration?: number;
  /**
   * Its keyframes, one or more, at different times.
   */
  keyframes: KeyframesStyleEntryKeyframe[];
}

/**
 * A keyframe of a `KeyframesStyleEntry`.
 */
export interface KeyframesStyleEntryKeyframe {
  /**
   * When, in milliseconds from the start of its entry.
   */
  time: number;
  /**
   * The value of the property then, relative to the layer that plays it
   * (see `KeyframesStyleEntry`): a number, or a hexadecimal color for the
   * colors.
   */
  value: number | string;
  /**
   * How the value goes on from here to the next keyframe (`ease` by
   * default).
   */
  easing?: MotionEasing;
  /**
   * The design token that sets its easing, see `Keyframe.appliedTokens`.
   */
  appliedTokens?: KeyframeTokens;
  /**
   * For `x` and `y` on a motion path: where the curve comes into the
   * keyframe from, as an offset from its value.
   */
  pathIn?: number;
  /**
   * For `x` and `y` on a motion path: where the curve leaves the keyframe
   * towards, as an offset from its value.
   */
  pathOut?: number;
}

/**
 * The design tokens that set settings of an animation style entry, by
 * their names (see `PresetStyleEntry.appliedTokens` and
 * `PresetAnimation.appliedTokens`): `start` and `duration` take
 * `duration` tokens, `easing` a `cubicBezier` or a `spring` one; the
 * `amount` of a `fade` an `opacity` one, of a `scale` a `number` one, of
 * a `rotate` a `rotation` or `number` one, of a `blur` a `dimension` or
 * `number` one; the offsets of a `move` `dimension` or `number` ones.
 */
export interface AnimationStyleEntryTokens {
  start?: string;
  duration?: string;
  easing?: string;
  amount?: string;
  offsetX?: string;
  offsetY?: string;
}

/**
 * A use of an animation style (`LibraryAnimationStyle`) by a layer, in a
 * `Timeline`: it plays a copy of the entries of the style, scaled to its
 * own span, and follows the edits of the style.
 */
export interface AppliedAnimationStyle {
  /**
   * The id of the use.
   */
  readonly id: string;
  /**
   * The board or layer that plays it.
   */
  readonly shape: Shape;
  /**
   * The style, while it is in the file or in a library of it, else null
   * (the use plays its copy).
   */
  readonly style: LibraryAnimationStyle | null;
  /**
   * The name of the style, as the use copied it.
   */
  readonly name: string;
  /**
   * When it starts, in milliseconds, before 0 too, where it does not play.
   */
  start: number;
  /**
   * How long it plays, in milliseconds: its entries scale to it.
   */
  duration: number;
  /**
   * Turns the use into the preset animations it plays, which no longer
   * follow the style.
   * @return Returns the preset animations.
   */
  detach(): PresetAnimation[];
  /**
   * Removes the use.
   */
  remove(): void;
}

/**
 * Represents shadow properties in Penpot.
 * This interface includes properties for defining drop shadows and inner shadows, along with their visual attributes.
 */
export interface Shadow {
  /**
   * The optional unique identifier for the shadow.
   */
  id?: string;
  /**
   * The optional style of the shadow.
   * - 'drop-shadow': A shadow cast outside the element.
   * - 'inner-shadow': A shadow cast inside the element.
   */
  style?: 'drop-shadow' | 'inner-shadow';
  /**
   * The optional X-axis offset of the shadow.
   */
  offsetX?: number;
  /**
   * The optional Y-axis offset of the shadow.
   */
  offsetY?: number;
  /**
   * The optional blur radius of the shadow.
   */
  blur?: number;
  /**
   * The optional spread radius of the shadow.
   */
  spread?: number;
  /**
   * Specifies whether the shadow is hidden.
   * Defaults to false if omitted.
   */
  hidden?: boolean;
  /**
   * The optional color of the shadow, defined by a Color object.
   */
  color?: Color;
}

/**
 * Shape represents a union of various shape types used in the Penpot project.
 * This type allows for different shapes to be handled under a single type umbrella.
 *
 * @example
 * ```js
 * let shape: Shape;
 * if (penpot.utils.types.isRectangle(shape)) {
 *   console.log(shape.type);
 * }
 * ```
 */
export type Shape =
  Board | Group | Boolean | Rectangle | Path | Text | Ellipse | SvgRaw | Image;

/**
 * Represents the base properties and methods of a shape in Penpot.
 * This interface provides common properties and methods shared by all shapes.
 */
export interface ShapeBase extends PluginData {
  /**
   * The unique identifier of the shape.
   */
  readonly id: string;

  /**
   * The name of the shape.
   */
  name: string;

  /**
   * The parent shape. If the shape is the first level the parent will be the root shape.
   * For the root shape the parent is null
   */
  readonly parent: Shape | null;

  /**
   * Returns the index of the current shape in the parent
   */
  readonly parentIndex: number;

  /**
   * The x-coordinate of the shape's position.
   */
  x: number;

  /**
   * The y-coordinate of the shape's position.
   */
  y: number;

  /**
   * The width of the shape.
   */
  readonly width: number;

  /**
   * The height of the shape.
   */
  readonly height: number;

  /**
   * The bounding box surrounding the current shape
   */
  readonly bounds: Bounds;

  /**
   * The geometric center of the shape
   */
  readonly center: Point;

  /**
   * Indicates whether the shape is blocked.
   */
  blocked: boolean;

  /**
   * Indicates whether the shape is hidden.
   */
  hidden: boolean;

  /**
   * Indicates whether the shape is visible.
   */
  visible: boolean;

  /**
   * Indicates whether the shape has proportion lock enabled.
   */
  proportionLock: boolean;

  /**
   * The horizontal constraints applied to the shape.
   */
  constraintsHorizontal: 'left' | 'right' | 'leftright' | 'center' | 'scale';

  /**
   * The vertical constraints applied to the shape.
   */
  constraintsVertical: 'top' | 'bottom' | 'topbottom' | 'center' | 'scale';

  /**
   * Indicates whether the shape stays fixed in place while scrolling.
   */
  fixedWhenScrolling: boolean;

  /**
   * The border radius of the shape.
   */
  borderRadius: number;

  /**
   * The border radius of the top-left corner of the shape.
   */
  borderRadiusTopLeft: number;

  /**
   * The border radius of the top-right corner of the shape.
   */
  borderRadiusTopRight: number;

  /**
   * The border radius of the bottom-right corner of the shape.
   */
  borderRadiusBottomRight: number;

  /**
   * The border radius of the bottom-left corner of the shape.
   */
  borderRadiusBottomLeft: number;

  /**
   * The opacity of the shape.
   */
  opacity: number;

  /**
   * The blend mode applied to the shape.
   */
  blendMode:
    | 'normal'
    | 'darken'
    | 'multiply'
    | 'color-burn'
    | 'lighten'
    | 'screen'
    | 'color-dodge'
    | 'overlay'
    | 'soft-light'
    | 'hard-light'
    | 'difference'
    | 'exclusion'
    | 'hue'
    | 'saturation'
    | 'color'
    | 'luminosity';

  /**
   * The shadows applied to the shape.
   */
  shadows: Shadow[];

  /**
   * The blur effect applied to the shape.
   */
  blur?: Blur;

  /**
   * The background blur effect applied to the shape.
   * Background blur creates a blur effect on the content behind the shape,
   * rather than on the shape's own content.
   */
  backgroundBlur?: Blur;

  /**
   * The export settings of the shape.
   */
  exports: Export[];

  /**
   * The x-coordinate of the shape relative to its board.
   */
  boardX: number;

  /**
   * The y-coordinate of the shape relative to its board.
   */
  boardY: number;

  /**
   * The x-coordinate of the shape relative to its parent.
   */
  parentX: number;

  /**
   * The y-coordinate of the shape relative to its parent.
   */
  parentY: number;

  /**
   * Indicates whether the shape is flipped horizontally.
   */
  flipX: boolean;

  /**
   * Indicates whether the shape is flipped vertically.
   */
  flipY: boolean;

  /**
   * The rotation in degrees of the shape with respect to its center.
   */
  rotation: number;

  /**
   * The 3D rotation in degrees of the shape about its own horizontal axis:
   * a positive value tilts its top edge away from the viewer, as CSS
   * `rotateX()` does. The perspective is that of its board (see
   * `Board.perspective`). Penpot Motion; 0 is flat.
   */
  rotateX: number;

  /**
   * The 3D rotation in degrees of the shape about its own vertical axis:
   * a positive value tilts its right edge away from the viewer, as CSS
   * `rotateY()` does. Penpot Motion; 0 is flat.
   */
  rotateY: number;

  /**
   * How far in pixels the shape comes towards the viewer, in the space of
   * its parent, as CSS `translateZ()`: nearer, it shows bigger and away
   * from the vanishing point of its board. Negative goes back. Penpot
   * Motion; 0 is in the plane. Animatable as the keyframe property
   * `translateZ`.
   */
  translateZ: number;

  /**
   * When true the shape hides while its back faces the viewer, as CSS
   * `backface-visibility: hidden`: a card that flips shows its back layer
   * only. The back is that of the shape as its tilt and the tilt of the
   * groups and boards it is in turn it.
   */
  backfaceHidden: boolean;

  /**
   * The fills applied to the shape.
   */
  fills: Fill[] | 'mixed';

  /**
   * The strokes applied to the shape.
   */
  strokes: Stroke[];

  /**
   * Layout properties for children of the shape.
   */
  readonly layoutChild?: LayoutChildProperties;

  /**
   * Layout properties for cells in a grid layout.
   */
  readonly layoutCell?: LayoutCellProperties;

  /**
   * Changes the index inside the parent of the current shape.
   * This method will shift the indexes of the shapes around that position to
   * match the index.
   * If the index is greater than the number of elements it will positioned last.
   *
   * @param index the new index for the shape to be in
   */
  setParentIndex(index: number): void;

  /**
   * The design tokens applied to this shape.
   * It's a map property name -> token name.
   *
   * NOTE that the tokens application is by name and not by id. If there exist
   * several tokens with the same name in different sets, the actual token applied
   * and the value set to the attributes will depend on which sets are active
   * (and will change if different sets or themes are activated later).
   */
  readonly tokens: { [property in TokenProperty]: string };

  /**
   * @return Returns true if the current shape is inside a component instance
   */
  isComponentInstance(): boolean;

  /**
   * @return Returns true if the current shape is inside a component **main** instance
   */
  isComponentMainInstance(): boolean;

  /**
   * @return Returns true if the current shape is inside a component **copy** instance
   */
  isComponentCopyInstance(): boolean;

  /**
   * @return Returns true when the current shape is the root of a component tree
   */
  isComponentRoot(): boolean;

  /**
   * @return Returns true when the current shape is the head of a components tree nested structure
   */
  isComponentHead(): boolean;

  /**
   * @return Returns the equivalent shape in the component main instance. If the current shape is inside a
   * main instance will return `null`;
   */
  componentRefShape(): Shape | null;

  /**
   * @return Returns the root of the component tree structure for the current shape. If the current shape
   * is already a root will return itself.
   */
  componentRoot(): Shape | null;

  /**
   * @return Returns the head of the component tree structure for the current shape. If the current shape
   * is already a head will return itself.
   */
  componentHead(): Shape | null;

  /**
   * @return If the shape is a component instance, returns the reference to the component associated
   * otherwise will return null
   */
  component(): LibraryComponent | null;

  /**
   * If the current shape is a component it will remove the component information and leave the
   * shape as a "basic shape"
   */
  detach(): void;

  /**
   * Swaps the component instance for another component, keeping the overrides
   * when possible. Similar to the "swap component" action on the Penpot interface.
   * The current shape must be a component copy instance.
   * @param component The new component to replace the current one
   */
  swapComponent(component: LibraryComponent): void;

  /**
   * Resets the overrides of the component copy, restoring all its attributes
   * (and those of its children) to the ones in the linked main component.
   * Similar to the "reset overrides" action on the Penpot interface.
   * The current shape must be a component copy instance.
   */
  resetOverrides(): void;

  /**
   * Applies this copy's overrides to its main component and synchronizes copies.
   * Similar to the editor's "update main component" action.
   * Requires `content:write` and `library:write` permissions.
   * The shape must be a component copy head on the active page of the current file,
   * outside any other component copy, with a main component that is not deleted.
   */
  updateMain(): void;

  /**
   * Switch a VariantComponent copy to the nearest one that has the specified property value
   * @param pos The position of the property to update
   * @param value The new value of the property
   */
  switchVariant(pos: number, value: string): void;

  /**
   * Combine several standard Components into a VariantComponent. Similar to doing it with the contextual menu
   * on the Penpot interface.
   * The current shape must be a component main instance.
   * @param ids A list of ids of the main instances of the components to combine with this one.
   * @return The variant container created
   */
  combineAsVariants(ids: string[]): VariantContainer;

  /**
   * @return Returns true when the current shape is the head of a components tree nested structure,
   * and that component is a VariantComponent
   */
  isVariantHead(): boolean;

  /**
   * Resizes the shape to the specified width and height.
   * @param width The new width of the shape.
   * @param height The new height of the shape.
   *
   * @example
   * ```js
   * shape.resize(200, 100);
   * ```
   */
  resize(width: number, height: number): void;

  /**
   * Rotates the shape in relation with the given center.
   * @param angle Angle in degrees to rotate.
   * @param center Center of the transform rotation. If not sent it will use the geometric center of the shape.
   *
   * @example
   * ```js
   * shape.rotate(45);
   * ```
   */
  rotate(angle: number, center?: { x: number; y: number } | null): void;

  /**
   * Moves the current shape to the front of its siblings
   */
  bringToFront(): void;

  /**
   * Moves the current shape one position forward in its list of siblings
   */
  bringForward(): void;

  /**
   * Moves the current shape to the back of its siblings
   */
  sendToBack(): void;

  /**
   * Moves the current shape one position backwards in its list of siblings
   */
  sendBackward(): void;

  /**
   * Generates an export from the current shape.
   *
   * @example
   * ```js
   * shape.export({ type: 'png', scale: 2 });
   * ```
   */
  export(config: Export): Promise<Uint8Array>;

  /**
   * The interactions for the current shape.
   */
  readonly interactions: Interaction[];

  /**
   * Adds a new interaction to the shape.
   *
   * If the interaction starts a flow (for example a `navigate-to` action) and
   * the shape's board is not already part of any flow, a new flow starting at
   * that board is created automatically, matching the behavior of the editor.
   *
   * @param trigger defines the conditions under which the action will be triggered
   * @param action defines what will be executed when the trigger happens
   * @param delay for the type of trigger `after-delay` will specify the time after triggered. Ignored otherwise.
   *
   * @example
   * ```js
   * shape.addInteraction('click', { type: 'navigate-to', destination: anotherBoard });
   * ```
   */
  addInteraction(trigger: Trigger, action: Action, delay?: number): Interaction;

  /**
   * Removes the interaction from the shape.
   * @param interaction is the interaction to remove from the shape
   *
   * @example
   * ```js
   * shape.removeInteraction(interaction);
   * ```
   */
  removeInteraction(interaction: Interaction): void;

  /**
   * Applies one design token to one or more properties of the shape.
   * @param token is the Token to apply
   * @param properties an optional list of property names. If omitted, the
   * default properties will be applied.
   *
   * NOTE that the tokens application is by name and not by id. If there exist
   * several tokens with the same name in different sets, the actual token applied
   * and the value set to the attributes will depend on which sets are active
   * (and will change if different sets or themes are activated later).
   */
  applyToken(token: Token, properties?: TokenProperty[]): void;

  /**
   * Creates a clone of the shape.
   * @return Returns a new instance of the shape with identical properties.
   */
  clone(): Shape;

  /**
   * Removes the shape from its parent.
   */
  remove(): void;

  /**
   * This method returns a promise that will be resolved when all the
   * pending layout updates have finished and the components have synchronized.
   * If no layout work is pending the promise resolves immediately.
   * @param timeout Maximum time to wait, in milliseconds. If the timeout
   * elapses before the shape's layout settles, the promise is rejected.
   * Defaults to 30000; the promise never waits indefinitely.
   * @return The promise to be resolved when the shape's layout is updated. It
   * is rejected with an Error, both on timeout and on an invalid timeout value.
   */
  waitForLayoutUpdate(timeout?: number): Promise<void>;
}

/**
 * Slide animation
 */
export interface Slide {
  /**
   * Type of the animation.
   */
  readonly type: 'slide';

  /**
   * Indicate if the slide will be either in-to-out `in` or out-to-in `out`.
   */
  readonly way: 'in' | 'out';

  /**
   * Direction for the slide animation.
   */
  readonly direction: 'right' | 'left' | 'up' | 'down';

  /**
   * Duration of the animation effect.
   */
  readonly duration: number;

  /**
   * If `true` the offset effect will be used.
   */
  readonly offsetEffect?: boolean;

  /**
   * Function that the slide effect will follow for the interpolation.
   * Defaults to `linear`.
   */
  readonly easing?: 'linear' | 'ease' | 'ease-in' | 'ease-out' | 'ease-in-out';
}

/**
 * Stroke properties for any shape except groups.
 * Literals apply strokeWidth before top/right/bottom/left widths.
 */
export interface Stroke {
  /**
   * The optional color of the stroke, represented as a string (e.g., '#FF5733').
   */
  strokeColor?: string;
  /**
   * The optional reference to an external file for the stroke color.
   */
  strokeColorRefFile?: string;
  /**
   * The optional reference ID within the external file for the stroke color.
   */
  strokeColorRefId?: string;
  /**
   * The optional opacity level of the stroke color, ranging from 0 (fully transparent) to 1 (fully opaque).
   * Defaults to 1 if omitted.
   */
  strokeOpacity?: number;
  /**
   * The optional style of the stroke.
   */
  strokeStyle?: 'solid' | 'dotted' | 'dashed' | 'mixed' | 'none' | 'svg';
  /**
   * Reads the top width. Setting it selects simple mode and sets all four sides.
   */
  strokeWidth?: number;
  /**
   * Computed width mode: simple when all four effective widths match, multiple otherwise.
   */
  readonly strokeWidthType?: 'simple' | 'multiple';
  /**
   * Top side width; setting it preserves the other side widths.
   * Boards and rectangles only; other shape types raise a validation error.
   */
  strokeWidthTop?: number;
  /**
   * Right side width; setting it preserves the other side widths.
   * Boards and rectangles only; other shape types raise a validation error.
   */
  strokeWidthRight?: number;
  /**
   * Bottom side width; setting it preserves the other side widths.
   * Boards and rectangles only; other shape types raise a validation error.
   */
  strokeWidthBottom?: number;
  /**
   * Left side width; setting it preserves the other side widths.
   * Boards and rectangles only; other shape types raise a validation error.
   */
  strokeWidthLeft?: number;
  /**
   * The optional alignment of the stroke relative to the shape's boundary.
   */
  strokeAlignment?: 'center' | 'inner' | 'outer';
  /**
   * The optional cap style for the start of the stroke.
   */
  strokeCapStart?: StrokeCap;
  /**
   * The optional cap style for the end of the stroke.
   */
  strokeCapEnd?: StrokeCap;
  /**
   * The optional gradient stroke defined by a Gradient object.
   */
  strokeColorGradient?: Gradient;
  /**
   * The optional image stroke defined by an ImageData object.
   */
  strokeImage?: ImageData;
}

/**
 * Represents the cap style of a stroke in Penpot.
 * This type defines various styles for the ends of a stroke.
 */
export type StrokeCap =
  | 'round'
  | 'square'
  | 'line-arrow'
  | 'triangle-arrow'
  | 'square-marker'
  | 'circle-marker'
  | 'diamond-marker';

/**
 * Represents an SVG raw shape in Penpot.
 * This interface extends `ShapeBase` and includes properties specific to raw SVG shapes.
 */
export interface SvgRaw extends ShapeBase {
  /**
   * The type of the shape, which is always 'svg-raw' for raw SVG shapes.
   */
  readonly type: 'svg-raw';
}

/**
 * Text represents a text element in the Penpot application, extending the base shape interface.
 * It includes various properties to define the text content and its styling attributes.
 */
export interface Text extends ShapeBase {
  /**
   * The type of the shape, which is always 'text' for text shapes.
   */
  readonly type: 'text';
  /**
   * The characters contained within the text shape.
   */
  characters: string;
  /**
   * The grow type of the text shape, defining how the text box adjusts its size.
   * Possible values are:
   * - 'fixed': Fixed size.
   * - 'auto-width': Adjusts width automatically.
   * - 'auto-height': Adjusts height automatically.
   */
  growType: 'fixed' | 'auto-width' | 'auto-height';

  /**
   * The font ID used in the text shape, or 'mixed' if multiple fonts are used.
   */
  fontId: string | 'mixed';

  /**
   * The font family used in the text shape, or 'mixed' if multiple font families are used.
   */
  fontFamily: string | 'mixed';

  /**
   * The font variant ID used in the text shape, or 'mixed' if multiple font variants are used.
   */
  fontVariantId: string | 'mixed';

  /**
   * The font size used in the text shape, or 'mixed' if multiple font sizes are used.
   */
  fontSize: string | 'mixed';

  /**
   * The font weight used in the text shape, or 'mixed' if multiple font weights are used.
   */
  fontWeight: string | 'mixed';

  /**
   * The font style used in the text shape, or 'mixed' if multiple font styles are used.
   */
  fontStyle: 'normal' | 'italic' | 'mixed' | null;

  /**
   * The line height used in the text shape, or 'mixed' if multiple line heights are used.
   */
  lineHeight: string | 'mixed';

  /**
   * The letter spacing used in the text shape, or 'mixed' if multiple letter spacings are used.
   */
  letterSpacing: string | 'mixed';

  /**
   * The text transform applied to the text shape, or 'mixed' if multiple text transforms are used.
   */
  textTransform: 'uppercase' | 'capitalize' | 'lowercase' | 'mixed' | null;

  /**
   * The text decoration applied to the text shape, or 'mixed' if multiple text decorations are used.
   */
  textDecoration: 'underline' | 'line-through' | 'mixed' | null;

  /**
   * The text direction for the text shape, or 'mixed' if multiple directions are used.
   */
  direction: 'ltr' | 'rtl' | 'mixed' | null;

  /**
   * The horizontal alignment of the text shape. It can be a specific alignment or 'mixed' if multiple alignments are used.
   */
  align: 'left' | 'center' | 'right' | 'justify' | 'mixed' | null;

  /**
   * The vertical alignment of the text shape.
   */
  verticalAlign: 'top' | 'center' | 'bottom' | null;

  /**
   * Return the bounding box for the text as a (x, y, width, height) rectangle
   * This is the box that covers the text even if it overflows its selection rectangle.
   */
  readonly textBounds: { x: number; y: number; width: number; height: number };

  /**
   * Gets a text range within the text shape.
   * @param start - The start index of the text range.
   * @param end - The end index of the text range.
   * @return Returns a TextRange object representing the specified text range.
   *
   * @example
   * ```js
   * const textRange = textShape.getRange(0, 10);
   * console.log(textRange.characters);
   * ```
   */
  getRange(start: number, end: number): TextRange;

  /**
   * Applies a typography style to the text shape.
   * @param typography - The typography style to apply.
   * @remarks
   * This method sets various typography properties for the text shape according to the given typography style.
   *
   * @example
   * ```js
   * textShape.applyTypography(typography);
   * ```
   */
  applyTypography(typography: LibraryTypography): void;
}

/**
 * Represents a range of text within a Text shape.
 * This interface provides properties for styling and formatting text ranges.
 */
export interface TextRange {
  /**
   * The Text shape to which this text range belongs.
   */
  readonly shape: Text;

  /**
   * The characters associated with the current text range.
   */
  readonly characters: string;

  /**
   * The font ID of the text range. It can be a specific font ID or 'mixed' if multiple fonts are used.
   */
  fontId: string | 'mixed';

  /**
   * The font family of the text range. It can be a specific font family or 'mixed' if multiple font families are used.
   */
  fontFamily: string | 'mixed';

  /**
   * The font variant ID of the text range. It can be a specific font variant ID or 'mixed' if multiple font variants are used.
   */
  fontVariantId: string | 'mixed';

  /**
   * The font size of the text range. It can be a specific font size or 'mixed' if multiple font sizes are used.
   */
  fontSize: string | 'mixed';

  /**
   * The font weight of the text range. It can be a specific font weight or 'mixed' if multiple font weights are used.
   */
  fontWeight: string | 'mixed';

  /**
   * The font style of the text range. It can be a specific font style or 'mixed' if multiple font styles are used.
   */
  fontStyle: 'normal' | 'italic' | 'mixed' | null;

  /**
   * The line height of the text range. It can be a specific line height or 'mixed' if multiple line heights are used.
   */
  lineHeight: string | 'mixed';

  /**
   * The letter spacing of the text range. It can be a specific letter spacing or 'mixed' if multiple letter spacings are used.
   */
  letterSpacing: string | 'mixed';

  /**
   * The text transform applied to the text range. It can be a specific text transform or 'mixed' if multiple text transforms are used.
   */
  textTransform:
    'uppercase' | 'capitalize' | 'lowercase' | 'none' | 'mixed' | null;

  /**
   * The text decoration applied to the text range. It can be a specific text decoration or 'mixed' if multiple text decorations are used.
   */
  textDecoration: 'underline' | 'line-through' | 'none' | 'mixed' | null;

  /**
   * The text direction for the text range. It can be a specific direction or 'mixed' if multiple directions are used.
   */
  direction: 'ltr' | 'rtl' | 'mixed' | null;

  /**
   * The fill styles applied to the text range.
   */
  fills: Fill[] | 'mixed';

  /**
   * The horizontal alignment of the text range. It can be a specific alignment or 'mixed' if multiple alignments are used.
   */
  align: 'left' | 'center' | 'right' | 'justify' | 'mixed' | null;

  /**
   * The vertical alignment of the text range. It can be a specific alignment or 'mixed' if multiple alignments are used.
   */
  verticalAlign: 'top' | 'center' | 'bottom' | 'mixed' | null;

  /**
   * Applies a typography style to the text range.
   * This method sets various typography properties for the text range according to the given typography style.
   * @param typography - The typography style to apply.
   *
   * @example
   * ```js
   * textRange.applyTypography(typography);
   * ```
   */
  applyTypography(typography: LibraryTypography): void;
}

/**
 * This type specifies the possible themes: 'light' or 'dark'.
 */
export type Theme = 'light' | 'dark';

/**
 * It opens an overlay if it is not already opened or closes it if it is already opened.
 */
export interface ToggleOverlay extends OverlayAction {
  /**
   * The action type
   */
  readonly type: 'toggle-overlay';
}

/**
 * Represents a track configuration in Penpot.
 * This interface includes properties for defining the type and value of a track used in layout configurations.
 */
export interface Track {
  /**
   * The type of the track.
   * This can be one of the following values:
   * - 'flex': A flexible track type.
   * - 'fixed': A fixed track type.
   * - 'percent': A track type defined by a percentage.
   * - 'auto': An automatic track type.
   */
  type: TrackType;
  /**
   * The value of the track.
   * This can be a number representing the size of the track, or null if not applicable.
   */
  value: number | null;
}

/**
 * Represents the type of track in Penpot.
 * This type defines various track types that can be used in layout configurations.
 */
export type TrackType = 'flex' | 'fixed' | 'percent' | 'auto';

/**
 * Types of triggers defined:
 * - `click` triggers when the user uses the mouse to click on a shape
 * - `mouse-enter` triggers when the user moves the mouse inside the shape (even if no mouse button is pressed)
 * - `mouse-leave` triggers when the user moves the mouse outside the shape.
 * - `after-delay` triggers after the `delay` time has passed even if no interaction from the user happens.
 */
export type Trigger = 'click' | 'mouse-enter' | 'mouse-leave' | 'after-delay';

/**
 * Represents the base properties and methods of a Design Token in Penpot, shared by
 * all token types.
 */
export interface TokenBase {
  /**
   * The unique identifier for this token, used only internally inside Penpot.
   * This one is not exported or synced with external Design Token sources.
   */
  readonly id: string;

  /**
   * The name of the token. It may include a group path separated by `.`.
   */
  name: string;

  /**
   * An optional description text.
   */
  description: string;

  /**
   * Adds to the set that contains this Token a new one equal to this one
   * but with a new id.
   */
  duplicate(): Token;

  /**
   * Removes this token from the catalog.
   *
   * It will NOT be unapplied from any shape, since there may be other tokens
   * with the same name.
   */
  remove(): void;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's converted to string, regardless of the data type of the value depending
   * on the token type. It can be undefined if no value has been found in active sets.
   */
  readonly resolvedValueString: string | undefined;

  /**
   * Applies this token to one or more properties of the given shapes.
   * @param shapes is an array of shapes to apply it.
   * @param properties an optional list of property names. If omitted, the
   * default properties will be applied.
   *
   * NOTE that the tokens application is by name and not by id. If there exist
   * several tokens with the same name in different sets, the actual token applied
   * and the value set to the attributes will depend on which sets are active
   * (and will change if different sets or themes are activated later).
   */
  applyToShapes(shapes: Shape[], properties?: TokenProperty[]): void;

  /**
   * Applies this token to the currently selected shapes.
   *
   * Parameters and warnings are the same as above.
   */
  applyToSelected(properties?: TokenProperty[]): void;
}

/**
 * Represents a token of type BorderRadius.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenBorderRadius extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'borderRadius';

  /**
   * The value as defined in the token itself.
   * It's a positive number or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a positive number, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * The value of a TokenShadow in its composite form.
 */
export interface TokenShadowValue {
  /**
   * The color as a string (e.g. "#FF5733").
   */
  color: string;

  /**
   * If the shadow is inset or drop.
   */
  inset: boolean;

  /**
   * The horizontal offset of the shadow in pixels.
   */
  offsetX: number;

  /**
   * The vertical offset of the shadow in pixels.
   */
  offsetY: number;

  /**
   * The spread distance of the shadow in pixels.
   */
  spread: number;

  /**
   * The amount of blur to apply to the shadow.
   */
  blur: number;
}

/**
 * The value of a TokenShadow in its composite of strings form.
 */
export interface TokenShadowValueString {
  /**
   * The color as a string (e.g. "#FF5733"), or a reference
   * to a color token.
   */
  color: string;

  /**
   * If the shadow is inset or drop, or a reference of a
   * boolean token.
   */
  inset: string;

  /**
   * The horizontal offset of the shadow in pixels, or a reference
   * to a number token.
   */
  offsetX: string;

  /**
   * The vertical offset of the shadow in pixels, or a reference
   * to a number token.
   */
  offsetY: string;

  /**
   * The spread distance of the shadow in pixels, or a reference
   * to a number token.
   */
  spread: string;

  /**
   * The amount of blur to apply to the shadow, or a reference
   * to a number token.
   */
  blur: string;
}

/**
 * Represents a token of type Shadow.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenShadow extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'shadow';

  /**
   * The value as defined in the token itself.
   * It may be a string with a reference to other token, or else
   * an array of TokenShadowValueString.
   */
  value: string | TokenShadowValueString[];

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's an array of TokenShadowValue, or undefined if no value has been found
   * in active sets.
   */
  readonly resolvedValue: TokenShadowValue[] | undefined;
}

/**
 * Represents a token of type Color.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenColor extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'color';

  /**
   * The value as defined in the token itself.
   * It's a rgb color or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a rgb color, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: string | undefined;
}

/**
 * Represents a token of type Dimension.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenDimension extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'dimension';

  /**
   * The value as defined in the token itself.
   * It's a positive number or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a positive number, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type FontFamilies.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenFontFamilies extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'fontFamilies';

  /**
   * The value as defined in the token itself.
   * It may be a string with a reference to other token, or else
   * an array of strings with one or more font families (each family
   * is an item in the array).
   */
  value: string | string[];

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's an array of strings with one or more font families,
   * or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: string[] | undefined;
}

/**
 * Represents a token of type FontSizes.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenFontSizes extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'fontSizes';

  /**
   * The value as defined in the token itself.
   * It's a positive number or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a positive number, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type FontWeights.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenFontWeights extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'fontWeights';

  /**
   * The value as defined in the token itself.
   * It's a weight string or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a weight string ("bold", "strong", etc.), or undefined if no value has
   * been found in active sets.
   */
  readonly resolvedValue: string | undefined;
}

/**
 * Represents a token of type LetterSpacing.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenLetterSpacing extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'letterSpacing';

  /**
   * The value as defined in the token itself.
   * It's a number or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a number, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type Duration (Penpot Motion): a length of time.
 * No shape takes it; preset animations and the entries of animation
 * styles bind it for their start and duration (see
 * `PresetAnimation.appliedTokens`).
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenDuration extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'duration';

  /**
   * The value as defined in the token itself: a number of milliseconds or
   * seconds (`"150ms"`, `"0.2s"`; milliseconds without a unit) or a
   * reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a number of milliseconds, or undefined if no value has been found
   * in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type Cubic Bezier (Penpot Motion): an easing curve
 * as CSS `cubic-bezier()` gives it. No shape takes it; preset
 * animations, the entries of animation styles and keyframes bind it for
 * their easing (see `PresetAnimation.appliedTokens` and
 * `Keyframe.appliedTokens`).
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenCubicBezier extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'cubicBezier';

  /**
   * The value as defined in the token itself: the four numbers
   * `[x1, y1, x2, y2]` of the control points, `x1` and `x2` from 0 to 1,
   * or a reference.
   */
  value: [number, number, number, number] | string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's the four numbers, or undefined if no value has been found in
   * active sets.
   */
  readonly resolvedValue: number[] | undefined;
}

/**
 * The settings of a spring, see `TokenSpring`.
 */
export interface TokenSpringValue {
  /**
   * How hard it pulls to the end value, more than 0 (`170` by default in
   * the easing editor).
   */
  stiffness: number;
  /**
   * How much it slows down, 0 or more (`26` by default).
   */
  damping: number;
  /**
   * How heavy the moving value is, more than 0 (`1` by default).
   */
  mass: number;
}

/**
 * Represents a token of type Spring (Penpot Motion): an easing that
 * settles like a spring. No shape takes it; the easing of preset
 * animations, of animation style entries and of keyframes binds it like
 * a `cubicBezier` token (see `PresetAnimation.appliedTokens` and
 * `Keyframe.appliedTokens`). Its DTCG `$type` is `spring`.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenSpring extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'spring';

  /**
   * The value as defined in the token itself: the settings of the spring,
   * as an object or a text with the three numbers (`"170, 26, 1"`), or a
   * reference.
   */
  value: TokenSpringValue | string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's the settings of the spring, or undefined if no value has been
   * found in active sets.
   */
  readonly resolvedValue: TokenSpringValue | undefined;
}

/**
 * Represents a token of type Number.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenNumber extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'number';

  /**
   * The value as defined in the token itself.
   * It's a number or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a number, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type Opacity.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenOpacity extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'opacity';

  /**
   * The value as defined in the token itself.
   * It's a number between 0 and 1 or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a number between 0 and 1, or undefined if no value has been found
   * in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type Rotation.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenRotation extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'rotation';

  /**
   * The value as defined in the token itself.
   * It's a number in degrees or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a number in degrees, or undefined if no value has been found
   * in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type Sizing.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenSizing extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'sizing';

  /**
   * The value as defined in the token itself.
   * It's a number or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a number, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type Spacing.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenSpacing extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'spacing';

  /**
   * The value as defined in the token itself.
   * It's a number or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a number, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type BorderWidth.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenBorderWidth extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'borderWidth';

  /**
   * The value as defined in the token itself.
   * It's a positive number or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a positive number, or undefined if no value has been found in active sets.
   */
  readonly resolvedValue: number | undefined;
}

/**
 * Represents a token of type TextCase.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenTextCase extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'textCase';

  /**
   * The value as defined in the token itself.
   * It's a case string or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a case string ("none", "uppercase", "lowercase", "capitalize"), or
   * undefined if no value has been found in active sets.
   */
  readonly resolvedValue: string | undefined;
}

/**
 * Represents a token of type Decoration.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenTextDecoration extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'textDecoration';

  /**
   * The value as defined in the token itself.
   * It's a decoration string or a reference.
   */
  value: string;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a decoration string, or undefined if no value has been found
   * in active sets.
   */
  readonly resolvedValue: string | undefined;
}

/**
 * The value of a TokenTypography in its composite form.
 */
export interface TokenTypographyValue {
  /**
   * The letter spacing, as a number.
   */
  letterSpacing: number;

  /**
   * The list of font families.
   */
  fontFamilies: string[];

  /**
   * The font size, as a positive number.
   */
  fontSizes: number;

  /**
   * The font weight, as a weight string ("bold", "strong", etc.).
   */
  fontWeights: string;

  /**
   * The line height, as a number.
   */
  lineHeight: number;

  /**
   * The text case as a string ("none", "uppercase", "lowercase" "capitalize").
   */
  textCase: string;

  /**
   * The text decoration as a string ("none", "underline", "strike-through").
   */
  textDecoration: string;
}

/**
 * The value of a TokenTypography in its composite of strings form.
 */
export interface TokenTypographyValueString {
  /**
   * The letter spacing, as a number, or a reference to a TokenLetterSpacing.
   */
  letterSpacing: string;

  /**
   * The list of font families, or a reference to a TokenFontFamilies.
   */
  fontFamilies: string | string[];

  /**
   * The font size, as a positive number, or a reference to a TokenFontSizes.
   */
  fontSizes: string;

  /**
   * The font weight, as a weight string ("bold", "strong", etc.), or a
   * reference to a TokenFontWeights.
   */
  fontWeight: string;

  /**
   * The line height, as a number. Note that there is no individual line-height
   * token type; it only exists as part of a Typography token. If you need to
   * put a reference here, use a Number token.
   */
  lineHeight: string;

  /**
   * The text case as a string ("none", "uppercase", "lowercase" "capitalize"),
   * or a reference to a TokenTextCase.
   */
  textCase: string;

  /**
   * The text decoration as a string ("none", "underline", "strike-through"),
   * or a reference to a TokenTextDecoration.
   */
  textDecoration: string;
}

/**
 * Represents a token of type Typography.
 * This interface extends `TokenBase` and specifies the data type of the value.
 */
export interface TokenTypography extends TokenBase {
  /**
   * The type of the token.
   */
  readonly type: 'typography';

  /**
   * The value as defined in the token itself.
   * It may be a string with a reference to other token, or a
   * TokenTypographyValueString.
   */
  value: string | TokenTypographyValueString;

  /**
   * The value calculated by finding all tokens with the same name in active sets
   * and resolving the references.
   *
   * It's a TokenTypographyValue, or undefined if no value has been found
   * in active sets.
   */
  readonly resolvedValue: TokenTypographyValue[] | undefined;
}

/**
 * Any possible type of value field in a token.
 *
 * Token values are always stored as strings, including for numeric token
 * types such as `spacing`, `dimension` or `borderRadius` (e.g. `"16"` or
 * `"16px"`). A plain `number` is also accepted on input and coerced to its
 * string representation.
 */
export type TokenValueString =
  | TokenShadowValueString
  | TokenTypographyValueString
  | TokenSpringValue
  | string
  | string[]
  | number
  | number[];

/**
 * The supported Design Tokens in Penpot.
 */
export type Token =
  | TokenBorderRadius
  | TokenShadow
  | TokenColor
  | TokenCubicBezier
  | TokenDimension
  | TokenDuration
  | TokenFontFamilies
  | TokenFontSizes
  | TokenFontWeights
  | TokenLetterSpacing
  | TokenNumber
  | TokenOpacity
  | TokenRotation
  | TokenSizing
  | TokenSpacing
  | TokenSpring
  | TokenBorderWidth
  | TokenTextCase
  | TokenTextDecoration
  | TokenTypography;

/**
 * The collection of all tokens in a Penpot file's library.
 *
 * Tokens are contained in sets, that can be marked as active
 * or inactive to control the resolved value of the tokens.
 *
 * The active status of sets can be handled by presets named
 * Themes.
 */
export interface TokenCatalog {
  /**
   * Returns true if the tokens can be edited (it can't
   * if the tokens of the file are in an external library).
   * Note that even in this case, Themes and Sets status still
   * can be changed and tokens can be applied to shapes.
   */
  readonly isEditableTokens: boolean;

  /**
   * The list of themes in this catalog, in creation order.
   */
  readonly themes: TokenTheme[];

  /**
   * The list of sets in this catalog, in the order defined
   * by the user. The order is important because when the same token name
   * exists in several active sets, the latter has precedence.
   */
  readonly sets: TokenSet[];

  /**
   * Creates a new TokenTheme and adds it to the catalog.
   * @param group The group name of the theme (can be empty string).
   * @param name The name of the theme (required)
   * @return Returns the created TokenTheme.
   */
  addTheme({ group, name }: { group: string; name: string }): TokenTheme;

  /**
   * Creates a new TokenSet and adds it to the catalog.
   *
   * Newly created sets are **inactive** by default: only active sets
   * affect shapes and reference resolution. Pass `active: true` to create
   * an already-active set, or activate it later via `set.active = true` /
   * `set.toggleActive()`.
   * @param name The name of the set (required). It may contain
   * a group path, separated by `/`.
   * @param active Whether the set should be activated on creation.
   * Defaults to `false`.
   * @return Returns the created TokenSet.
   */
  addSet({ name, active }: { name: string; active?: boolean }): TokenSet;

  /**
   * Retrieves a theme.
   * @param id the id of the theme.
   * @returns Returns the theme or undefined if not found.
   */
  getThemeById(id: string): TokenTheme | undefined;

  /**
   * Retrieves a set.
   * @param id the id of the set.
   * @returns Returns the set or undefined if not found.
   */
  getSetById(id: string): TokenSet | undefined;
}

/**
 * A collection of Design Tokens.
 *
 * Inside a set, tokens have an unique name, that will designate
 * what token to use if the name is applied to a shape and this
 * set is active.
 */
export interface TokenSet {
  /**
   * The unique identifier for this set, used only internally inside Penpot.
   * This one is not exported or synced with external Design Token sources.
   */
  readonly id: string;

  /**
   * The name of the set. It may include a group path separated by `/`.
   */
  name: string;

  /**
   * Indicates if the set is currently active.
   */
  active: boolean;

  /**
   * The tokens contained in this set, in alphabetical order.
   */
  readonly tokens: Token[];

  /**
   * The tokens contained in this set, grouped by type.
   */
  readonly tokensByType: [string, Token[]][];

  /**
   * Toggles the active status of this set.
   */
  toggleActive(): void;

  /**
   * Retrieves a token.
   * @param id the id of the token.
   * @returns Returns the token or undefined if not found.
   */
  getTokenById(id: string): Token | undefined;

  /**
   * Creates a new Token and adds it to the set.
   * @param type The type of the token.
   * @param name The name of the token (required). It may contain
   * a group path, separated by `.`.
   * @param value The value of the token (required), always in its string
   * form. This applies to numeric token types too (e.g. `spacing`,
   * `dimension`, `borderRadius`): use `"16"` or `"16px"` rather than `16`.
   * For convenience a plain number is also accepted and coerced to its
   * string representation (`16` becomes `"16"`).
   * @return Returns the created Token.
   */
  addToken({
    type,
    name,
    value,
  }: {
    type: TokenType;
    name: string;
    value: TokenValueString;
  }): Token;

  /**
   * Adds to the catalog a new TokenSet equal to this one but with a new id.
   */
  duplicate(): TokenSet;

  /**
   * Removes this set from the catalog.
   */
  remove(): void;
}

/**
 * A preset of active TokenSets.
 *
 * A theme contains a list of references to TokenSets. When the theme
 * is activated, it sets are activated too. This will not deactivate
 * sets that are _not_ in this theme, because they may have been
 * activated by other themes.
 *
 * Themes may be grouped. At any time only one of the themes in a group
 * may be active. But there may be active themes in other groups. This
 * allows defining multiple "axis" for theming (e.g. color scheme,
 * density or brand).
 *
 * When a TokenSet is activated or deactivated directly, all themes
 * are disabled (indicating that now there is a "custom" manual theme
 * active).
 */
export interface TokenTheme {
  /**
   * The unique identifier for this theme, used only internally inside Penpot.
   * This one is not exported or synced with external Design Token sources.
   */
  readonly id: string;

  /**
   * Optional identifier that may exist if the theme was imported from an
   * external tool that uses ids in the json file.
   */
  readonly externalId: string | undefined;

  /**
   * The group name of the theme. Can be an empty string.
   */
  group: string;

  /**
   * The name of the theme.
   */
  name: string;

  /**
   * Indicates if the theme is currently active.
   */
  active: boolean;

  /**
   * Toggles the active status of this theme.
   */
  toggleActive(): void;

  /**
   * The sets that will be activated if this theme is activated.
   */
  readonly activeSets: TokenSet[];

  /**
   * Adds a set to the list of the theme.
   *
   * @param tokenSet a `TokenSet` or the id of a token set.
   */
  addSet(tokenSet: TokenSet | string): void;

  /**
   * Removes a set from the list of the theme.
   *
   * @param tokenSet a `TokenSet` or the id of a token set.
   */
  removeSet(tokenSet: TokenSet | string): void;

  /**
   * Adds to the catalog a new TokenTheme equal to this one but with a new id.
   */
  duplicate(): TokenTheme;

  /**
   * Removes this theme from the catalog.
   */
  remove(): void;
}

/**
 * The properties that a BorderRadius token can be applied to.
 */
type TokenBorderRadiusProps =
  | 'borderRadiusTopLeft'
  | 'borderRadiusTopRight'
  | 'borderRadiusBottomRight'
  | 'borderRadiusBottomLeft';

/**
 * The properties that a Shadow token can be applied to.
 */
type TokenShadowProps = 'shadow';

/**
 * The properties that a Color token can be applied to.
 */
type TokenColorProps = 'fill' | 'strokeColor';

/**
 * The properties that a Dimension token can be applied to.
 */
type TokenDimensionProps =
  // Axis
  | 'x'
  | 'y'

  // Stroke width
  | 'strokeWidth'
  | 'strokeWidthTop'
  | 'strokeWidthRight'
  | 'strokeWidthBottom'
  | 'strokeWidthLeft';

/**
 * The properties that a FontFamilies token can be applied to.
 */
type TokenFontFamiliesProps = 'fontFamilies';

/**
 * The properties that a FontSizes token can be applied to.
 */
type TokenFontSizesProps = 'fontSize';

/**
 * The properties that a FontWeight token can be applied to.
 */
type TokenFontWeightProps = 'fontWeight';

/**
 * The properties that a LetterSpacing token can be applied to.
 */
type TokenLetterSpacingProps = 'letterSpacing';

/**
 * The properties that a Number token can be applied to.
 */
type TokenNumberProps = 'rotation';

/**
 * The properties that an Opacity token can be applied to.
 */
type TokenOpacityProps = 'opacity';

/**
 * The properties that a Sizing token can be applied to.
 */
type TokenSizingProps =
  // Size
  | 'width'
  | 'height'

  // Layout
  | 'layoutItemMinW'
  | 'layoutItemMaxW'
  | 'layoutItemMinH'
  | 'layoutItemMaxH';

/**
 * The properties that a Spacing token can be applied to.
 */
type TokenSpacingProps =
  // Spacing / Gap
  | 'rowGap'
  | 'columnGap'

  // Spacing / Padding
  | 'paddingLeft'
  | 'paddingTop'
  | 'paddingRight'
  | 'paddingBottom'

  // Spacing / Margin
  | 'marginLeft'
  | 'marginTop'
  | 'marginRight'
  | 'marginBottom';

/**
 * The properties that a BorderWidth token can be applied to.
 */
type TokenBorderWidthProps =
  | 'strokeWidth'
  | 'strokeWidthTop'
  | 'strokeWidthRight'
  | 'strokeWidthBottom'
  | 'strokeWidthLeft';

/**
 * The properties that a TextCase token can be applied to.
 */
type TokenTextCaseProps = 'textCase';

/**
 * The properties that a TextDecoration token can be applied to.
 */
type TokenTextDecorationProps = 'textDecoration';

/**
 * The properties that a Typography token can be applied to.
 */
type TokenTypographyProps = 'typography';

/**
 * All the properties that a token can be applied to.
 * Not always correspond to Shape properties. For example,
 * `fill` property applies to `fillColor` of the first fill
 * of the shape.
 *
 */
export type TokenProperty =
  | 'all'
  | TokenBorderRadiusProps
  | TokenShadowProps
  | TokenColorProps
  | TokenDimensionProps
  | TokenFontFamiliesProps
  | TokenFontSizesProps
  | TokenFontWeightProps
  | TokenLetterSpacingProps
  | TokenNumberProps
  | TokenOpacityProps
  | TokenSizingProps
  | TokenSpacingProps
  | TokenBorderWidthProps
  | TokenTextCaseProps
  | TokenTextDecorationProps
  | TokenTypographyProps;

/**
 * The supported types of Design Tokens in Penpot.
 */
export type TokenType =
  | 'borderRadius'
  | 'shadow'
  | 'color'
  | 'cubicBezier'
  | 'dimension'
  | 'duration'
  | 'fontFamilies'
  | 'fontSizes'
  | 'fontWeights'
  | 'letterSpacing'
  | 'number'
  | 'opacity'
  | 'rotation'
  | 'sizing'
  | 'spacing'
  | 'spring'
  | 'borderWidth'
  | 'textCase'
  | 'textDecoration'
  | 'typography';

/**
 * Represents a user in Penpot.
 */
export interface User {
  /**
   * The unique identifier of the user.
   *
   * @example
   * ```js
   * const userId = user.id;
   * console.log(userId);
   * ```
   */
  readonly id: string;

  /**
   * The name of the user.
   *
   * @example
   * ```js
   * const userName = user.name;
   * console.log(userName);
   * ```
   */
  readonly name?: string;

  /**
   * The URL of the user's avatar image.
   *
   * @example
   * ```js
   * const avatarUrl = user.avatarUrl;
   * console.log(avatarUrl);
   * ```
   */
  readonly avatarUrl?: string;

  /**
   * The color associated with the user.
   *
   * @example
   * ```js
   * const userColor = user.color;
   * console.log(userColor);
   * ```
   */
  readonly color: string;

  /**
   * The session ID of the user.
   *
   * @example
   * ```js
   * const sessionId = user.sessionId;
   * console.log(sessionId);
   * ```
   */
  readonly sessionId?: string;
}

/**
 * Represents a Variant in Penpot: the grouping of all the VariantComponents that
 * belong to the same VariantContainer, together with their shared properties.
 * This interface provides attributes and actions that affect the Variant as a
 * whole (not a single VariantComponent).
 */
export interface Variants {
  /**
   * The unique identifier of the variant element. It is the id of the VariantContainer, and all the VariantComponents
   * that belong to this variant have an attribute variantId which this is as value.
   */
  readonly id: string;

  /**
   * The unique identifier of the library to which the variant belongs.
   */
  readonly libraryId: string;

  /**
   * A list with the names of the properties of the Variant
   */
  readonly properties: string[];

  /**
   * A list of all the values of a property across all the VariantComponents of this Variant
   * @param property The name of the property
   */
  currentValues(property: string): string[];

  /**
   * Remove a property of the Variant
   * @param pos The position of the property to remove
   */
  removeProperty(pos: number): void;

  /**
   * Rename a property of the Variant
   * @param pos The position of the property to rename
   * @param name The new name of the property
   */
  renameProperty(pos: number, name: string): void;

  /**
   * List all the VariantComponents on this Variant.
   */
  variantComponents(): LibraryComponent[];

  /**
   * Creates a duplicate of the main VariantComponent of this Variant
   */
  addVariant(): void;

  /**
   * Adds a new property to this Variant
   */
  addProperty(): void;
}

/**
 * Viewport represents the viewport in the Penpot application.
 * It includes the center point, zoom level, and the bounds of the viewport.
 */
export interface Viewport {
  /**
   * the `center` point of the current viewport. If changed will change the
   * viewport position.
   */
  center: Point;

  /**
   * the `zoom` level as a number where `1` represents 100%.
   */
  zoom: number;

  /**
   * the `bounds` are the current coordinates of the viewport.
   */
  readonly bounds: Bounds;

  /**
   * Resets the zoom level.
   */
  zoomReset(): void;

  /**
   * Changes the viewport and zoom so can fit all the current shapes in the page.
   */
  zoomToFitAll(): void;

  /**
   * Changes the viewport and zoom so all the `shapes` in the argument are
   * visible.
   */
  zoomIntoView(shapes: Shape[]): void;
}

declare global {
  const penpot: Penpot;
  const penpotMgmt: PenpotMgmt;
}
