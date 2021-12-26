package org.im97mori.ble.android.peripheral.utils;

import static android.text.Spanned.SPAN_EXCLUSIVE_EXCLUSIVE;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;

import android.os.Build;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;

import org.im97mori.ble.android.peripheral.R;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class AfterTextChangedTextWatcherTest {

    @Test
    public void test_beforeTextChanged_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        AfterTextChangedTextWatcher watcher = new AfterTextChangedTextWatcher(s -> result.set(true));
        watcher.beforeTextChanged(null, 0, 0, 0);
        assertFalse(result.get());
    }

    @Test
    public void test_onTextChanged_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        AfterTextChangedTextWatcher watcher = new AfterTextChangedTextWatcher(s -> result.set(true));
        watcher.onTextChanged(null, 0, 0, 0);
        assertFalse(result.get());
    }

    @Test
    public void test_afterTextChanged_00001() {
        final AtomicReference<String> result = new AtomicReference<>(null);
        SpannableStringBuilder ssb = new SpannableStringBuilder();
        AfterTextChangedTextWatcher watcher = new AfterTextChangedTextWatcher(result::set);
        watcher.afterTextChanged(ssb);
        assertEquals(ssb.toString(), result.get());
    }

    @Test
    public void test_afterTextChanged_00002() {
        final AtomicReference<String> result = new AtomicReference<>("abc");
        SpannableStringBuilder ssb = new SpannableStringBuilder(result.get());
        ssb.setSpan(new ForegroundColorSpan(R.color.teal_700), 0, 1, SPAN_EXCLUSIVE_EXCLUSIVE);
        AfterTextChangedTextWatcher watcher = new AfterTextChangedTextWatcher(result::set);
        watcher.afterTextChanged(ssb);
        assertEquals(ssb.toString(), result.get());
    }

}