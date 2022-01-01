package org.im97mori.ble.android.peripheral.ui.device;

import static junit.framework.TestCase.assertFalse;
import static org.junit.Assert.assertTrue;

import android.os.Build;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.concurrent.atomic.AtomicBoolean;

import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class StateChangeServerCallbackTest {

    @Test
    public void onServerStarted() {
        AtomicBoolean result = new AtomicBoolean(false);
        StateChangeServerCallback callback = new StateChangeServerCallback(result::set);
        callback.onServerStarted();
        assertTrue(result.get());
    }

    @Test
    public void onServerStopped() {
        AtomicBoolean result = new AtomicBoolean(false);
        StateChangeServerCallback callback = new StateChangeServerCallback(result::set);
        callback.onServerStopped();
        assertFalse(result.get());
    }

}