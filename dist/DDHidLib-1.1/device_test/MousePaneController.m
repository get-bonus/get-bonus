/*
 * Copyright (c) 2007 Dave Dribin
 * 
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#import "MousePaneController.h"
#import "DDHidLib.h"
#import "ButtonState.h"


@interface MousePaneController (Private)

- (void) setMouseX: (int) mouseX;
- (void) setMouseY: (int) mouseY;
- (void) setMouseWheel: (int) mouseWheel;

@end

@implementation MousePaneController

static int sMaxValue = 2500;

static int applyDelta(int current, int delta)
{
    int newValue = (current + delta) % sMaxValue;
    if (newValue < 0)
        newValue = sMaxValue + newValue;
    return newValue;
}

- (void) awakeFromNib;
{
    mCurrentMouse = 0;
    mMouseButtons = [[NSMutableArray alloc] init];

    NSArray * mice = [DDHidMouse allMice];
    [mice makeObjectsPerformSelector: @selector(setDelegate:)
                          withObject: self];
    [self setMice: mice];
    [self setMouseIndex: 0];
}

//=========================================================== 
//  mice 
//=========================================================== 
- (NSArray *) mice
{
    return mMice; 
}

- (void) setMice: (NSArray *) theMice
{
    if (mMice != theMice)
    {
        [mMice release];
        mMice = [theMice retain];
    }
}

- (NSArray *) mouseButtons;
{
    return mMouseButtons;
}

- (BOOL) no;
{
    return NO;
}

//=========================================================== 
// - mouseIndex
//=========================================================== 
- (unsigned) mouseIndex
{
    return mMouseIndex;
}

//=========================================================== 
// - setMouseIndex:
//=========================================================== 
- (void) setMouseIndex: (unsigned) theMouseIndex
{
    if (mCurrentMouse != nil)
    {
        [mCurrentMouse stopListening];
        mCurrentMouse = nil;
    }
    mMouseIndex = theMouseIndex;
    [mMiceController setSelectionIndex: mMouseIndex];
    if (mMouseIndex != NSNotFound)
    {
        mCurrentMouse = [mMice objectAtIndex: mMouseIndex];
        [mCurrentMouse startListening];
        [self setMouseX: sMaxValue/2];
        [self setMouseY: sMaxValue/2];
        [self setMouseWheel: sMaxValue/2];

        [self willChangeValueForKey: @"mouseButtons"];
        [mMouseButtons removeAllObjects];
        NSArray * buttons = [mCurrentMouse buttonElements];
        NSEnumerator * e = [buttons objectEnumerator];
        DDHidElement * element;
        while (element = [e nextObject])
        {
            ButtonState * state = [[ButtonState alloc] initWithName: [[element usage] usageName]];
            [state autorelease];
            [mMouseButtons addObject: state];
        }
        [self didChangeValueForKey: @"mouseButtons"];
    }
}

- (int) maxValue;
{
    return sMaxValue;
}

//=========================================================== 
// - mouseX
//=========================================================== 
- (int) mouseX
{
    return mMouseX;
}

//=========================================================== 
// - mouseY
//=========================================================== 
- (int) mouseY
{
    return mMouseY;
}

//=========================================================== 
// - mouseWheel
//=========================================================== 
- (int) mouseWheel
{
    return mMouseWheel;
}

- (void) ddhidMouse: (DDHidMouse *) mouse xChanged: (SInt32) deltaX;
{
    [self setMouseX: applyDelta(mMouseX, deltaX)];
}

- (void) ddhidMouse: (DDHidMouse *) mouse yChanged: (SInt32) deltaY;
{
    [self setMouseY: applyDelta(mMouseY, deltaY)];
}

- (void) ddhidMouse: (DDHidMouse *) mouse wheelChanged: (SInt32) deltaWheel;
{
    // Some wheels only output -1 or +1, some output a more analog value.
    // Normalize wheel to -1%/+1% movement.
    deltaWheel = (deltaWheel/abs(deltaWheel))*(sMaxValue/100);
    [self setMouseWheel: applyDelta(mMouseWheel, deltaWheel)];
}

- (void) ddhidMouse: (DDHidMouse *) mouse buttonDown: (unsigned) buttonNumber;
{
    ButtonState * state = [mMouseButtons objectAtIndex: buttonNumber];
    [state setPressed: YES];
}

- (void) ddhidMouse: (DDHidMouse *) mouse buttonUp: (unsigned) buttonNumber;
{
    ButtonState * state = [mMouseButtons objectAtIndex: buttonNumber];
    [state setPressed: NO];
}

@end

@implementation MousePaneController (Private)

- (void) setMouseX: (int) mouseX;
{
    mMouseX = mouseX;
}

- (void) setMouseY: (int) mouseY;
{
    mMouseY = mouseY;
}

- (void) setMouseWheel: (int) mouseWheel;
{
    mMouseWheel = mouseWheel;
}

@end

