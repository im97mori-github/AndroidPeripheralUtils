package org.im97mori.ble.android.peripheral.utils;

import static junit.framework.TestCase.assertEquals;

import android.content.Context;
import android.os.Build;
import android.widget.AutoCompleteTextView;
import android.widget.EditText;

import com.google.gson.Gson;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class UtilsTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_createGsonInstance_00001() {
        Gson gson1 = Utils.createGsonInstance();
        Gson gson2 = Utils.createGsonInstance();
        assertEquals(gson1, gson2);
    }

    @Test
    public void test_setTextDistinct_00001() {
        EditText editText = new EditText(mContext);
        CharSequence original = editText.getText();
        Utils.setTextDistinct(editText, null);
        assertEquals(original.toString(), editText.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00002() {
        EditText editText = new EditText(mContext);
        CharSequence original = editText.getText();
        Utils.setTextDistinct(editText, original);
        assertEquals(original.toString(), editText.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00003() {
        EditText editText = new EditText(mContext);
        CharSequence original = editText.getText();
        CharSequence modified = original + "a";
        Utils.setTextDistinct(editText, modified);
        assertEquals(modified.toString(), editText.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00004() {
        AutoCompleteTextView autoCompleteTextView = new AutoCompleteTextView(mContext);
        CharSequence original = autoCompleteTextView.getText();
        Utils.setTextDistinct(autoCompleteTextView, null);
        assertEquals(original.toString(), autoCompleteTextView.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00005() {
        AutoCompleteTextView autoCompleteTextView = new AutoCompleteTextView(mContext);
        CharSequence original = autoCompleteTextView.getText();
        Utils.setTextDistinct(autoCompleteTextView, original);
        assertEquals(original.toString(), autoCompleteTextView.getText().toString());
    }

    @Test
    public void test_setTextDistinct_00006() {
        AutoCompleteTextView autoCompleteTextView = new AutoCompleteTextView(mContext);
        CharSequence original = autoCompleteTextView.getText();
        CharSequence modified = original + "a";
        Utils.setTextDistinct(autoCompleteTextView, modified);
        assertEquals(modified.toString(), autoCompleteTextView.getText().toString());
    }
}