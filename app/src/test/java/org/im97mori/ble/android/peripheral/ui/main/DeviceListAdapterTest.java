package org.im97mori.ble.android.peripheral.ui.main;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;

import android.content.Context;
import android.graphics.Bitmap;
import android.os.Build;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.TextView;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.test.TestUtils;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
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
public class DeviceListAdapterTest {

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
    public void test_setDeviceList_00001() {
        List<DeviceSetting> list = new ArrayList<>();
        DeviceListAdapter adapter = new DeviceListAdapter(mContext, mDeviceRepository.provideDeviceTypeImageResMap(), list);
        assertTrue(adapter.isEmpty());

        adapter.setDeviceList(Collections.singletonList(new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));

        assertFalse(adapter.isEmpty());
    }

    @Test
    public void test_setDeviceList_00002() {
        DeviceListAdapter adapter = new DeviceListAdapter(mContext, mDeviceRepository.provideDeviceTypeImageResMap(), Collections.singletonList(new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)));
        assertFalse(adapter.isEmpty());

        adapter.setDeviceList(Collections.emptyList());

        assertTrue(adapter.isEmpty());
    }

    @Test
    public void test_getItemId_00001() {
        DeviceSetting deviceSetting = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        DeviceListAdapter adapter = new DeviceListAdapter(mContext, mDeviceRepository.provideDeviceTypeImageResMap(), Collections.singletonList(deviceSetting));
        assertEquals(deviceSetting.getId(), adapter.getItemId(0));
    }

    @Test
    public void test_getItemId_00002() {
        DeviceSetting deviceSetting1 = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        DeviceSetting deviceSetting2 = new DeviceSetting(2, "b", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        DeviceListAdapter adapter = new DeviceListAdapter(mContext, mDeviceRepository.provideDeviceTypeImageResMap(), Arrays.asList(deviceSetting1, deviceSetting2));
        assertEquals(deviceSetting2.getId(), adapter.getItemId(1));
    }

    @Test
    public void test_getView_00001() {
        DeviceSetting deviceSetting = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        Map<Integer, Integer> map = mDeviceRepository.provideDeviceTypeImageResMap();
        DeviceListAdapter adapter = new DeviceListAdapter(mContext, map, Collections.singletonList(deviceSetting));
        FrameLayout frameLayout = new FrameLayout(mContext);
        View view = adapter.getView(0, null, frameLayout);
        TextView textView = view.findViewById(R.id.grid_text);
        assertEquals(deviceSetting.getDeviceSettingName(), textView.getText().toString());
        Bitmap bitmap = TestUtils.getBitmap(mContext.getDrawable(Objects.requireNonNull(map.get(deviceSetting.getDeviceType()))));

        assertTrue(bitmap.sameAs(TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[1])));
    }

    @Test
    public void test_getView_00002() {
        DeviceSetting deviceSetting1 = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        DeviceSetting deviceSetting2 = new DeviceSetting(2, "b", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        Map<Integer, Integer> map = mDeviceRepository.provideDeviceTypeImageResMap();
        DeviceListAdapter adapter = new DeviceListAdapter(mContext, map, Arrays.asList(deviceSetting1, deviceSetting2));
        FrameLayout frameLayout = new FrameLayout(mContext);
        View view = adapter.getView(1, null, frameLayout);
        TextView textView = view.findViewById(R.id.grid_text);
        assertEquals(deviceSetting2.getDeviceSettingName(), textView.getText().toString());
        Bitmap bitmap = TestUtils.getBitmap(mContext.getDrawable(Objects.requireNonNull(map.get(deviceSetting2.getDeviceType()))));

        assertTrue(bitmap.sameAs(TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[1])));
    }

}
