package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBack;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.intent.Intents.intended;
import static androidx.test.espresso.intent.Intents.intending;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static androidx.test.espresso.matcher.ViewMatchers.hasDescendant;
import static androidx.test.espresso.matcher.ViewMatchers.isChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isEnabled;
import static androidx.test.espresso.matcher.ViewMatchers.isNotChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isNotEnabled;
import static androidx.test.espresso.matcher.ViewMatchers.withEffectiveVisibility;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static org.im97mori.ble.android.peripheral.test.TestUtils.getCurrentMethodName;
import static org.im97mori.ble.constants.CharacteristicUUID.MANUFACTURER_NAME_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.SYSTEM_ID_CHARACTERISTIC;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mockStatic;

import android.app.Activity;
import android.app.Instrumentation;
import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattService;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.espresso.Espresso;
import androidx.test.espresso.intent.Intents;
import androidx.test.espresso.matcher.ViewMatchers;

import com.google.android.material.appbar.MaterialToolbar;

import junit.framework.TestCase;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a23.SystemIdSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a24.ModelNumberStringSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a29.ManufacturerNameStringSettingActivity;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.u2a23.SystemId;
import org.im97mori.ble.characteristic.u2a24.ModelNumberString;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.LinkedList;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class DeviceInformationServiceSettingActivityTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<DeviceInformationServiceSettingActivity> mScenario;

    private FakeDeviceInformationServiceSettingViewModel mViewModel;

    private static MockedStatic<AutoDisposeViewModelProvider> mockedStatic;

    @Inject
    @ApplicationContext
    Context mContext;

    @BeforeClass
    public static void setUpClass() {
        mockedStatic = mockStatic(AutoDisposeViewModelProvider.class);
        mockedStatic.when(() -> AutoDisposeViewModelProvider.getViewModelClass(DeviceInformationServiceSettingViewModel.class))
                .thenReturn(FakeDeviceInformationServiceSettingViewModel.class);
    }

    @AfterClass
    public static void tearDownClass() {
        mockedStatic.close();
    }

    @Before
    public void setUp() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mHiltRule.inject();
        Intents.init();
    }

    @After
    public void tearDown() {
        Intents.release();
        mScenario.close();
    }

    @Test
    public void test_title_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.device_information_service))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_menu_save_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isNotEnabled()));
    }

    @Test
    public void test_menu_save_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).perform(click());

        ServiceData serviceData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        systemIdCharacteristicData.uuid = SYSTEM_ID_CHARACTERISTIC;
        systemIdCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        systemIdCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;

        systemIdCharacteristicData.data = new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes();
        systemIdCharacteristicData.delay = 1;
        serviceData.characteristicDataList.add(systemIdCharacteristicData);

        String originalModelNumberString = "a";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        serviceData.characteristicDataList.add(modelNumberStringCharacteristicData);

        String originalManufacturerNameString = "a";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1);
        serviceData.characteristicDataList.add(manufacturerNameStringCharacteristicData);

        byte[] data = Utils.parcelableToByteArray(serviceData);
        Intent original = new Intent();
        original.putExtra(DEVICE_INFORMATION_SERVICE.toString(), data);
        mViewModel.mObserveSaveSubject.onNext(original);

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_OK, activityResult.getResultCode());
        Intent resultData = activityResult.getResultData();
        assertNotNull(resultData);
        assertArrayEquals(data, resultData.getByteArrayExtra(DEVICE_INFORMATION_SERVICE.toString()));
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        TestCase.assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_activity_result_1_00001() {
        Intent resultData = new Intent();
        byte[] after = Utils.parcelableToByteArray(new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , new SystemId(1, 2).getBytes()
                , -1));
        resultData.putExtra(SYSTEM_ID_CHARACTERISTIC.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), SystemIdSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.setSystemIdData(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.systemIdSettingButton).performClick());
        Espresso.onIdle();

        assertArrayEquals(after, mViewModel.getSystemIdData());
    }

    @Test
    public void test_activity_result_1_00002() {
        Intent resultData = new Intent();
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), SystemIdSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        byte[] before = Utils.parcelableToByteArray(new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , new SystemId(1, 2).getBytes()
                , -1));
        mViewModel.setSystemIdData(before);

        mScenario.onActivity(activity -> activity.findViewById(R.id.systemIdSettingButton).performClick());
        Espresso.onIdle();

        assertNull(mViewModel.getSystemIdData());
    }

    @Test
    public void test_activity_result_2_00001() {
        Intent resultData = new Intent();
        byte[] after = Utils.parcelableToByteArray(new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , new ModelNumberString("").getBytes()
                , -1));
        resultData.putExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ModelNumberStringSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.setModelNumberStringData(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.modelNumberStringSettingButton).performClick());
        Espresso.onIdle();

        assertArrayEquals(after, mViewModel.getModelNumberStringData());
    }

    @Test
    public void test_activity_result_2_00002() {
        Intent resultData = new Intent();
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ModelNumberStringSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.setModelNumberStringData(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.modelNumberStringSettingButton).performClick());
        Espresso.onIdle();

        assertNull(mViewModel.getModelNumberStringData());
    }

    @Test
    public void test_activity_result_3_00001() {
        Intent resultData = new Intent();
        byte[] after = Utils.parcelableToByteArray(new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , new ModelNumberString("").getBytes()
                , -1));
        resultData.putExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ManufacturerNameStringSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.setManufacturerNameStringData(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.manufacturerNameStringSettingButton).performClick());
        Espresso.onIdle();

        assertEquals(after, mViewModel.getManufacturerNameStringData());
    }

    @Test
    public void test_activity_result_3_00002() {
        Intent resultData = new Intent();
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ManufacturerNameStringSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        byte[] before = Utils.parcelableToByteArray(new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , new ModelNumberString("").getBytes()
                , -1));
        mViewModel.setManufacturerNameStringData(before);

        mScenario.onActivity(activity -> activity.findViewById(R.id.manufacturerNameStringSettingButton).performClick());
        Espresso.onIdle();

        assertNull(mViewModel.getManufacturerNameStringData());
    }

    @Test
    public void test_recreate_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isSystemIdSupported_title_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.isSystemIdSupported)).check(matches(withText(R.string.system_id_support)));
    }

    @Test
    public void test_isSystemIdSupported_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsSystemIdSupportedConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isSystemIdSupported)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_isSystemIdSupported_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsSystemIdSupportedConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.isSystemIdSupported)).perform(click());

        assertFalse(result.get());
    }

    @Test
    public void test_systemIdCardView_visibility_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_systemIdCardView_visibility_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_systemIdCardView_visibility_00003() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_systemIdCardView_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.systemIdCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_systemIdCardView_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.systemIdCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_systemIdCardView_title_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.systemIdCardViewTitle)).check(matches(withText(R.string.system_id)));
    }

    @Test
    public void test_manufacturerIdentifierTitle_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.manufacturerIdentifierTitle)).check(matches(withText(R.string.manufacturer_identifier)));
    }

    @Test
    public void test_manufacturerIdentifier_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.manufacturerIdentifier)).check(matches(withText("")));
    }

    @Test
    public void test_manufacturerIdentifier_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(systemIdCharacteristicData));

        onView(withId(R.id.manufacturerIdentifier)).check(matches(withText(String.valueOf(originalManufacturerIdentifier))));
    }

    @Test
    public void test_organizationallyUniqueIdentifierTitle_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.organizationallyUniqueIdentifierTitle)).check(matches(withText(R.string.organizationally_unique_identifier)));
    }

    @Test
    public void test_organizationallyUniqueIdentifier_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.organizationallyUniqueIdentifier)).check(matches(withText("")));
    }

    @Test
    public void test_organizationallyUniqueIdentifier_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(systemIdCharacteristicData));

        onView(withId(R.id.organizationallyUniqueIdentifier)).check(matches(withText(String.valueOf(originalOrganizationallyUniqueIdentifier))));
    }

    @Test
    public void test_systemIdSettingButton_text_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.systemIdSettingButton)).check(matches(withText(R.string.setting)));
    }

    @Test
    public void test_systemIdSettingButton_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.systemIdSettingButton)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), SystemIdSettingActivity.class)));
        intended(hasExtra(SYSTEM_ID_CHARACTERISTIC.toString(), (String) null));
    }

    @Test
    public void test_systemIdSettingButton_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.systemIdSettingButton)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), SystemIdSettingActivity.class)));
        intended(hasExtra(SYSTEM_ID_CHARACTERISTIC.toString(), new byte[]{1}));
    }

    @Test
    public void test_modelNumberStringCardView_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.modelNumberStringCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_modelNumberStringCardView_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.modelNumberStringCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_modelNumberStringCardView_title_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.modelNumberStringCardViewTitle)).check(matches(withText(R.string.model_number_string)));
    }

    @Test
    public void test_modelNumberString_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.modelNumberString)).check(matches(withText("")));
    }

    @Test
    public void test_modelNumberString_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        String originalModelNumberString = "a";
        CharacteristicData modelNumberStringCharacteristicData = new CharacteristicData(MODEL_NUMBER_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ModelNumberString(originalModelNumberString).getBytes()
                , -1);
        mViewModel.setModelNumberStringData(Utils.parcelableToByteArray(modelNumberStringCharacteristicData));

        onView(withId(R.id.modelNumberString)).check(matches(withText(originalModelNumberString)));
    }

    @Test
    public void test_modelNumberStringSettingButton_text_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.modelNumberStringSettingButton)).check(matches(withText(R.string.setting)));
    }

    @Test
    public void test_modelNumberStringSettingButton_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.modelNumberStringSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ModelNumberStringSettingActivity.class)));
        intended(hasExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), (String) null));
    }

    @Test
    public void test_modelNumberStringSettingButton_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.modelNumberStringSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ModelNumberStringSettingActivity.class)));
        intended(hasExtra(MODEL_NUMBER_STRING_CHARACTERISTIC.toString(), new byte[]{1}));
    }

    @Test
    public void test_manufacturerNameStringCardView_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.manufacturerNameStringCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_manufacturerNameStringCardView_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.manufacturerNameStringCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_manufacturerNameStringCardView_title_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.manufacturerNameStringCardViewTitle)).check(matches(withText(R.string.manufacturer_name_string)));
    }

    @Test
    public void test_manufacturerNameString_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.manufacturerNameString)).check(matches(withText("")));
    }

    @Test
    public void test_manufacturerNameString_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        String originalManufacturerNameString = "b";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1);
        mViewModel.setManufacturerNameStringData(Utils.parcelableToByteArray(manufacturerNameStringCharacteristicData));

        onView(withId(R.id.manufacturerNameString)).check(matches(withText(originalManufacturerNameString)));
    }

    @Test
    public void test_manufacturerNameStringSettingButton_text_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.manufacturerNameStringSettingButton)).check(matches(withText(R.string.setting)));
    }

    @Test
    public void test_manufacturerNameStringSettingButton_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.manufacturerNameStringSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ManufacturerNameStringSettingActivity.class)));
        intended(hasExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString(), (String) null));
    }

    @Test
    public void test_manufacturerNameStringSettingButton_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.manufacturerNameStringSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ManufacturerNameStringSettingActivity.class)));
        intended(hasExtra(MANUFACTURER_NAME_STRING_CHARACTERISTIC.toString(), new byte[]{1}));
    }

    @Test
    public void test_recreate_systemIdCardView_visibility_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_systemIdCardView_visibility_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.systemIdCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_systemIdCardView_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.systemIdCardView)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.systemIdCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_systemIdCardView_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.systemIdCardView)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.systemIdCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_manufacturerIdentifier_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.manufacturerIdentifier)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.manufacturerIdentifier)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_manufacturerIdentifier_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(systemIdCharacteristicData));

        onView(withId(R.id.manufacturerIdentifier)).check(matches(withText(String.valueOf(originalManufacturerIdentifier))));

        mScenario.recreate();

        onView(withId(R.id.manufacturerIdentifier)).check(matches(withText(String.valueOf(originalManufacturerIdentifier))));
    }

    @Test
    public void test_recreate_organizationallyUniqueIdentifier_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.organizationallyUniqueIdentifier)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.organizationallyUniqueIdentifier)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_organizationallyUniqueIdentifier_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        long originalManufacturerIdentifier = 1;
        int originalOrganizationallyUniqueIdentifier = 2;
        CharacteristicData systemIdCharacteristicData = new CharacteristicData(SYSTEM_ID_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new SystemId(originalManufacturerIdentifier, originalOrganizationallyUniqueIdentifier).getBytes()
                , -1);
        mViewModel.setSystemIdData(Utils.parcelableToByteArray(systemIdCharacteristicData));

        onView(withId(R.id.organizationallyUniqueIdentifier)).check(matches(withText(String.valueOf(originalOrganizationallyUniqueIdentifier))));

        mScenario.recreate();

        onView(withId(R.id.organizationallyUniqueIdentifier)).check(matches(withText(String.valueOf(originalOrganizationallyUniqueIdentifier))));
    }

    @Test
    public void test_recreate_modelNumberStringCardView_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.modelNumberStringCardView)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.modelNumberStringCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_modelNumberStringCardView_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.modelNumberStringCardView)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.modelNumberStringCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_manufacturerNameStringCardView_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.manufacturerNameStringCardView)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.manufacturerNameStringCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_manufacturerNameStringCardView_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.manufacturerNameStringCardView)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.manufacturerNameStringCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_manufacturerNameString_00001() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        onView(withId(R.id.manufacturerNameString)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.manufacturerNameString)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_manufacturerNameString_00002() {
        Intent intent = new Intent(mContext, DeviceInformationServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceInformationServiceSettingViewModel.class));

        String originalManufacturerNameString = "b";
        CharacteristicData manufacturerNameStringCharacteristicData = new CharacteristicData(MANUFACTURER_NAME_STRING_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 1
                , new ManufacturerNameString(originalManufacturerNameString).getBytes()
                , -1);
        mViewModel.setManufacturerNameStringData(Utils.parcelableToByteArray(manufacturerNameStringCharacteristicData));

        onView(withId(R.id.manufacturerNameString)).check(matches(withText(originalManufacturerNameString)));

        mScenario.recreate();

        onView(withId(R.id.manufacturerNameString)).check(matches(withText(originalManufacturerNameString)));
    }

}