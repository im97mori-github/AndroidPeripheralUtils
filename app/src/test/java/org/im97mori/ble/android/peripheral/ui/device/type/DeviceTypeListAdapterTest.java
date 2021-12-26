package org.im97mori.ble.android.peripheral.ui.device.type;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;

import android.content.Context;
import android.graphics.Bitmap;
import android.os.Build;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.TextView;

import androidx.core.util.Pair;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.test.TestUtils;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;

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
public class DeviceTypeListAdapterTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    DeviceRepository mDeviceRepository;

    @Before
    public void setUp() {
        mHiltRule.inject();
    }

    @Test
    public void test_getView_00001() {
        Pair<Integer, String> pair = Pair.create(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, "a");
        Map<Integer, Integer> map = mDeviceRepository.provideDeviceTypeImageResMap();
        DeviceTypeListAdapter adapter = new DeviceTypeListAdapter(mContext, mDeviceRepository.provideDeviceTypeImageResMap(), Collections.singletonList(pair));

        FrameLayout frameLayout = new FrameLayout(mContext);
        View view = adapter.getView(0, null, frameLayout);
        TextView textView = (TextView) view;
        assertEquals(pair.second, textView.getText().toString());
        Bitmap bitmap = TestUtils.getBitmap(mContext.getDrawable(Objects.requireNonNull(map.get(pair.first))));

        assertTrue(bitmap.sameAs(TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[0])));
    }

    @Test
    public void test_getView_00002() {
        Pair<Integer, String> pair1 = Pair.create(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, "a");
        Pair<Integer, String> pair2 = Pair.create(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, "b");
        Map<Integer, Integer> map = mDeviceRepository.provideDeviceTypeImageResMap();
        DeviceTypeListAdapter adapter = new DeviceTypeListAdapter(mContext, map, Arrays.asList(pair1, pair2));
        FrameLayout frameLayout = new FrameLayout(mContext);
        View view = adapter.getView(1, null, frameLayout);
        TextView textView = (TextView) view;
        assertEquals(pair2.second, textView.getText().toString());
        Bitmap bitmap = TestUtils.getBitmap(mContext.getDrawable(Objects.requireNonNull(map.get(pair2.first))));

        assertTrue(bitmap.sameAs(TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[0])));
    }

}
