package org.im97mori.ble.android.peripheral.ui.device.setting.u1810;

import android.app.Activity;
import android.app.Instrumentation;
import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattService;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.widget.CheckBox;
import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.espresso.Espresso;
import androidx.test.espresso.intent.Intents;
import androidx.test.espresso.matcher.ViewMatchers;
import com.google.android.material.appbar.MaterialToolbar;
import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import dagger.hilt.android.testing.UninstallModules;
import dagger.hilt.components.SingletonComponent;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;
import junit.framework.TestCase;
import org.im97mori.ble.BLEUtils;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.module.ViewModelFactoryFunctionModule;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.FakeViewModelProviderFactoryFunction;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a35.BloodPressureMeasurementSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a36.IntermediateCuffPressureSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2a49.BloodPressureFeatureSettingActivity;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.core.IEEE_11073_20601_SFLOAT;
import org.im97mori.ble.characteristic.u2a35.BloodPressureMeasurement;
import org.im97mori.ble.characteristic.u2a36.IntermediateCuffPressure;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.LinkedList;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBack;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.intent.Intents.intended;
import static androidx.test.espresso.intent.Intents.intending;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static androidx.test.espresso.matcher.ViewMatchers.*;
import static org.im97mori.ble.android.peripheral.test.TestUtils.getCurrentMethodName;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.*;
import static org.im97mori.ble.constants.CharacteristicUUID.*;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.junit.Assert.*;

/** @noinspection UnnecessaryLocalVariable*/
@SuppressWarnings("ConstantConditions")
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
@UninstallModules(ViewModelFactoryFunctionModule.class)
public class BloodPressureServiceSettingActivityTest {

    @Module
    @InstallIn(SingletonComponent.class)
    interface FakeViewModelFactoryFunctionModule {
        @Singleton
        @Provides
        public static Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> bindViewModelProviderFactoryFunction() {
            FakeViewModelProviderFactoryFunction fakeViewModelProviderFactoryFunction = new FakeViewModelProviderFactoryFunction();
            fakeViewModelProviderFactoryFunction.setFakeViewModelClass(BloodPressureServiceSettingViewModel.class, FakeBloodPressureServiceSettingViewModel.class);
            return fakeViewModelProviderFactoryFunction;
        }
    }

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<BloodPressureServiceSettingActivity> mScenario;

    private FakeBloodPressureServiceSettingViewModel mViewModel;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    FakeDeviceSettingRepository mFakeDeviceSettingRepository;

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
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.blood_pressure_service))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_menu_save_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isNotEnabled()));
    }

    @Test
    public void test_menu_save_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).perform(click());

        ServiceData serviceData = new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());

        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , bloodPressureMeasurement.getBytes()
                , -1);
        serviceData.characteristicDataList.add(bloodPressureMeasurementCharacteristicData);

        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , intermediateCuffPressure.getBytes()
                , -1);
        serviceData.characteristicDataList.add(intermediateCuffPressureCharacteristicData);

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        serviceData.characteristicDataList.add(bloodPressureFeatureCharacteristicData);

        byte[] data = Utils.parcelableToByteArray(serviceData);
        Intent original = new Intent();
        original.putExtra(BLOOD_PRESSURE_SERVICE.toString(), data);
        mViewModel.mObserveSaveSubject.onNext(original);

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_OK, activityResult.getResultCode());
        Intent resultData = activityResult.getResultData();
        assertNotNull(resultData);
        assertArrayEquals(data, resultData.getByteArrayExtra(BLOOD_PRESSURE_SERVICE.toString()));
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        TestCase.assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_activity_result_1_00001() {
        Intent resultData = new Intent();
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , bloodPressureMeasurement.getBytes()
                , -1);
        byte[] after = Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData);
        resultData.putExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureMeasurementSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.setBloodPressureMeasurementData(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.bloodPressureMeasurementSettingButton).performClick());
        Espresso.onIdle();

        assertArrayEquals(after, mViewModel.getBloodPressureMeasurementData());
    }

    @Test
    public void test_activity_result_1_00002() {
        Intent resultData = new Intent();
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureMeasurementSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , bloodPressureMeasurement.getBytes()
                , -1);
        byte[] before = Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData);
        mViewModel.setBloodPressureMeasurementData(before);

        mScenario.onActivity(activity -> activity.findViewById(R.id.bloodPressureMeasurementSettingButton).performClick());
        Espresso.onIdle();

        assertNull(mViewModel.getBloodPressureMeasurementData());
    }

    @Test
    public void test_activity_result_2_00001() {
        Intent resultData = new Intent();
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , bloodPressureMeasurement.getBytes()
                , -1);
        byte[] after = Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData);
        resultData.putExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), IntermediateCuffPressureSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.setIntermediateCuffPressureData(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.intermediateCuffPressureSettingButton).performClick());
        Espresso.onIdle();

        assertArrayEquals(after, mViewModel.getIntermediateCuffPressureData());
    }

    @Test
    public void test_activity_result_2_00002() {
        Intent resultData = new Intent();
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), IntermediateCuffPressureSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , bloodPressureMeasurement.getBytes()
                , -1);
        byte[] before = Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData);
        mViewModel.setIntermediateCuffPressureData(before);

        mScenario.onActivity(activity -> activity.findViewById(R.id.intermediateCuffPressureSettingButton).performClick());
        Espresso.onIdle();

        assertNull(mViewModel.getIntermediateCuffPressureData());
    }

    @Test
    public void test_activity_result_3_00001() {
        Intent resultData = new Intent();
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , bloodPressureMeasurement.getBytes()
                , -1);
        byte[] after = Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData);
        resultData.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureFeatureSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.setBloodPressureFeatureData(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.bloodPressureFeatureSettingButton).performClick());
        Espresso.onIdle();

        assertArrayEquals(after, mViewModel.getBloodPressureFeatureData());
    }

    @Test
    public void test_activity_result_3_00002() {
        Intent resultData = new Intent();
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureFeatureSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , bloodPressureMeasurement.getBytes()
                , -1);
        byte[] before = Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData);
        mViewModel.setBloodPressureFeatureData(before);

        mScenario.onActivity(activity -> activity.findViewById(R.id.bloodPressureFeatureSettingButton).performClick());
        Espresso.onIdle();

        assertNull(mViewModel.getBloodPressureFeatureData());
    }

    @Test
    public void test_recreate_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_bloodPressureMeasurementCardView_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bloodPressureMeasurementCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_bloodPressureMeasurementCardView_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bloodPressureMeasurementCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_bloodPressureMeasurementCardViewTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementCardViewTitle)).check(matches(withText(R.string.blood_pressure_measurement)));
    }

    @Test
    public void test_bloodPressureMeasurementFlagsTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementFlagsTitle)).check(matches(withText(R.string.flags)));
    }

    @Test
    public void test_bloodPressureMeasurementFlags_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementFlags)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureMeasurementFlags_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementFlags)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementFlags, 2))));
    }

    @Test
    public void test_bloodPressureMeasurementSystolicTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementSystolicTitle)).check(matches(withText(R.string.systolic)));
    }

    @Test
    public void test_bloodPressureMeasurementSystolic_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementSystolic)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureMeasurementSystolic_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementSystolic)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueSystolicMmhg.getSfloat()))));
    }

    @Test
    public void test_bloodPressureMeasurementDiastolicTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementDiastolicTitle)).check(matches(withText(R.string.diastolic)));
    }

    @Test
    public void test_bloodPressureMeasurementDiastolic_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementDiastolic)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureMeasurementDiastolic_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementDiastolic)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueDiastolicMmhg.getSfloat()))));
    }

    @Test
    public void test_bloodPressureMeasurementMeanArterialPressureTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementMeanArterialPressureTitle)).check(matches(withText(R.string.mean_arterial_pressure)));
    }

    @Test
    public void test_bloodPressureMeasurementMeanArterialPressure_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementMeanArterialPressure)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureMeasurementMeanArterialPressure_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementMeanArterialPressure)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg.getSfloat()))));
    }

    @Test
    public void test_bloodPressureMeasurementTimeStampTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementTimeStampTitle)).check(matches(withText(R.string.time_stamp)));
    }

    @Test
    public void test_bloodPressureMeasurementTimeStamp_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementTimeStamp)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureMeasurementTimeStamp_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_TIME_STAMP_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementTimeStamp)).check(matches(withText(mFakeDeviceSettingRepository.getDateTimeString(bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds))));
    }

    @Test
    public void test_bloodPressureMeasurementPulseRateTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementPulseRateTitle)).check(matches(withText(R.string.pulse_rate)));
    }

    @Test
    public void test_bloodPressureMeasurementPulseRate_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementPulseRate)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureMeasurementPulseRate_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_PULSE_RATE_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementPulseRate)).check(matches(withText(String.valueOf(bloodPressureMeasurementPulseRate.getSfloat()))));
    }

    @Test
    public void test_bloodPressureMeasurementUserIdTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementUserIdTitle)).check(matches(withText(R.string.user_id)));
    }

    @Test
    public void test_bloodPressureMeasurementUserId_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementUserId)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureMeasurementUserId_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_USER_ID_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementUserId)).check(matches(withText(String.valueOf(bloodPressureMeasurementUserId))));
    }

    @Test
    public void test_bloodPressureMeasurementMeasurementStatusTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementMeasurementStatusTitle)).check(matches(withText(R.string.measurement_status)));
    }

    @Test
    public void test_bloodPressureMeasurementMeasurementStatus_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementMeasurementStatus)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureMeasurementMeasurementStatus_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_MEASUREMENT_STATUS_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementMeasurementStatus)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementMeasurementStatusFlags, 4))));
    }

    @Test
    public void test_bloodPressureMeasurementSettingButton_text_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementSettingButton)).check(matches(withText(R.string.setting)));
    }

    @Test
    public void test_bloodPressureMeasurementSettingButton_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.bloodPressureMeasurementSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureMeasurementSettingActivity.class)));
        intended(hasExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), (String) null));
    }

    @Test
    public void test_bloodPressureMeasurementSettingButton_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.bloodPressureMeasurementSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureMeasurementSettingActivity.class)));
        intended(hasExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), new byte[]{1}));
    }

    @Test
    public void test_isIntermediateCuffPressureSupported_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.isIntermediateCuffPressureSupported)).check(matches(withText(R.string.intermediate_cuff_pressure)));
    }

    @Test
    public void test_isIntermediateCuffPressureSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsIntermediateCuffPressureSupportedConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((CheckBox) activity.findViewById(R.id.isIntermediateCuffPressureSupported)).toggle());

        assertTrue(result.get());
    }

    @Test
    public void test_isIntermediateCuffPressureSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsIntermediateCuffPressureSupportedConsumer = result::set;
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mScenario.onActivity(activity -> ((CheckBox) activity.findViewById(R.id.isIntermediateCuffPressureSupported)).toggle());

        assertFalse(result.get());
    }

    @Test
    public void test_intermediateCuffPressureCardView_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_intermediateCuffPressureCardView_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_intermediateCuffPressureCardView_visibility_00003() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_intermediateCuffPressureCardView_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_intermediateCuffPressureCardView_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_intermediateCuffPressureCardViewTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureCardViewTitle)).check(matches(withText(R.string.intermediate_cuff_pressure)));
    }

    @Test
    public void test_intermediateCuffPressureFlagsTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureFlagsTitle)).check(matches(withText(R.string.flags)));
    }

    @Test
    public void test_intermediateCuffPressureFlags_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureFlags)).check(matches(withText("")));
    }

    @Test
    public void test_intermediateCuffPressureFlags_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureFlags)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureFlags, 2))));
    }

    @Test
    public void test_intermediateCuffPressureCurrentCuffPressureTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureCurrentCuffPressureTitle)).check(matches(withText(R.string.current_cuff_pressure)));
    }

    @Test
    public void test_intermediateCuffPressureCurrentCuffPressure_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureCurrentCuffPressure)).check(matches(withText("")));
    }

    @Test
    public void test_intermediateCuffPressureCurrentCuffPressure_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureCurrentCuffPressure)).check(matches(withText(String.valueOf(intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg.getSfloat()))));
    }

    @Test
    public void test_intermediateCuffPressureTimeStampTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureTimeStampTitle)).check(matches(withText(R.string.time_stamp)));
    }

    @Test
    public void test_intermediateCuffPressureTimeStamp_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureTimeStamp)).check(matches(withText("")));
    }

    @Test
    public void test_intermediateCuffPressureTimeStamp_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_TIME_STAMP_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureTimeStamp)).check(matches(withText(mFakeDeviceSettingRepository.getDateTimeString(intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds))));
    }

    @Test
    public void test_intermediateCuffPressurePulseRateTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressurePulseRateTitle)).check(matches(withText(R.string.pulse_rate)));
    }

    @Test
    public void test_intermediateCuffPressurePulseRate_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressurePulseRate)).check(matches(withText("")));
    }

    @Test
    public void test_intermediateCuffPressurePulseRate_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_PULSE_RATE_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressurePulseRate)).check(matches(withText(String.valueOf(intermediateCuffPressurePulseRate.getSfloat()))));
    }

    @Test
    public void test_intermediateCuffPressureUserIdTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureUserIdTitle)).check(matches(withText(R.string.user_id)));
    }

    @Test
    public void test_intermediateCuffPressureUserId_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureUserId)).check(matches(withText("")));
    }

    @Test
    public void test_intermediateCuffPressureUserId_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_USER_ID_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureUserId)).check(matches(withText(String.valueOf(intermediateCuffPressureUserId))));
    }

    @Test
    public void test_intermediateCuffPressureMeasurementStatusTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureMeasurementStatusTitle)).check(matches(withText(R.string.measurement_status)));
    }

    @Test
    public void test_intermediateCuffPressureMeasurementStatus_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureMeasurementStatus)).check(matches(withText("")));
    }

    @Test
    public void test_intermediateCuffPressureMeasurementStatus_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_MEASUREMENT_STATUS_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureMeasurementStatus)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureMeasurementStatusFlags, 4))));
    }

    @Test
    public void test_intermediateCuffPressureSettingButton_text_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureSettingButton)).check(matches(withText(R.string.setting)));
    }

    @Test
    public void test_intermediateCuffPressureSettingButton_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.intermediateCuffPressureSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), IntermediateCuffPressureSettingActivity.class)));
        intended(hasExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString(), (String) null));
    }

    @Test
    public void test_intermediateCuffPressureSettingButton_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.intermediateCuffPressureSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), IntermediateCuffPressureSettingActivity.class)));
        intended(hasExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString(), new byte[]{1}));
    }

    @Test
    public void test_bloodPressureFeatureCardView_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bloodPressureFeatureCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_bloodPressureFeatureCardView_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bloodPressureFeatureCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_bloodPressureFeatureCardViewTitle_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureFeatureCardViewTitle)).check(matches(withText(R.string.blood_pressure_feature)));
    }

    @Test
    public void test_bloodPressureFeature_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureFeature)).check(matches(withText("")));
    }

    @Test
    public void test_bloodPressureFeature_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        mViewModel.setBloodPressureFeatureData(Utils.parcelableToByteArray(bloodPressureFeatureCharacteristicData));

        onView(withId(R.id.bloodPressureFeature)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureFeature.getBytes(), 0), 4))));
    }

    @Test
    public void test_bloodPressureFeatureSettingButton_text_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureFeatureSettingButton)).check(matches(withText(R.string.setting)));
    }

    @Test
    public void test_bloodPressureFeatureSettingButton_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.bloodPressureFeatureSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureFeatureSettingActivity.class)));
        intended(hasExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), (String) null));
    }

    @Test
    public void test_bloodPressureFeatureSettingButton_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> activity.findViewById(R.id.bloodPressureFeatureSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureFeatureSettingActivity.class)));
        intended(hasExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), new byte[]{1}));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementCardView_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bloodPressureMeasurementCardView)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementCardView_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bloodPressureMeasurementCardView)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementFlags_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementFlags)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementFlags)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementFlags_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementFlags)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementFlags, 2))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementFlags)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementFlags, 2))));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementSystolic_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementSystolic)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementSystolic)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementSystolic_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementSystolic)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueSystolicMmhg.getSfloat()))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementSystolic)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueSystolicMmhg.getSfloat()))));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementDiastolic_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementDiastolic)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementDiastolic)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementDiastolic_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementDiastolic)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueDiastolicMmhg.getSfloat()))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementDiastolic)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueDiastolicMmhg.getSfloat()))));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementMeanArterialPressure_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementMeanArterialPressure)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementMeanArterialPressure)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementMeanArterialPressure_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementMeanArterialPressure)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg.getSfloat()))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementMeanArterialPressure)).check(matches(withText(String.valueOf(bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg.getSfloat()))));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementTimeStamp_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementTimeStamp)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementTimeStamp)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementTimeStamp_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_TIME_STAMP_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementTimeStamp)).check(matches(withText(mFakeDeviceSettingRepository.getDateTimeString(bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementTimeStamp)).check(matches(withText(mFakeDeviceSettingRepository.getDateTimeString(bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds))));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementPulseRate_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementPulseRate)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementTimeStamp)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementPulseRate_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_PULSE_RATE_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementPulseRate)).check(matches(withText(String.valueOf(bloodPressureMeasurementPulseRate.getSfloat()))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementPulseRate)).check(matches(withText(String.valueOf(bloodPressureMeasurementPulseRate.getSfloat()))));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementUserId_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementUserId)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementUserId)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementUserId_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_USER_ID_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementUserId)).check(matches(withText(String.valueOf(bloodPressureMeasurementUserId))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementUserId)).check(matches(withText(String.valueOf(bloodPressureMeasurementUserId))));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementMeasurementStatus_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureMeasurementMeasurementStatus)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementMeasurementStatus)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureMeasurementMeasurementStatus_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_INDICATE
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_MEASUREMENT_STATUS_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementData(Utils.parcelableToByteArray(bloodPressureMeasurementCharacteristicData));

        onView(withId(R.id.bloodPressureMeasurementMeasurementStatus)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementMeasurementStatusFlags, 4))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureMeasurementMeasurementStatus)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementMeasurementStatusFlags, 4))));
    }

    @Test
    public void test_recreate_intermediateCuffPressureCardView_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_recreate_intermediateCuffPressureCardView_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName() + 1);
        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_intermediateCuffPressureCardView_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_intermediateCuffPressureCardView_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_intermediateCuffPressureFlags_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureFlags)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureFlags)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_intermediateCuffPressureFlags_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureFlags)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureFlags, 2))));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureFlags)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureFlags, 2))));
    }

    @Test
    public void test_recreate_intermediateCuffPressureCurrentCuffPressure_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureCurrentCuffPressure)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureCurrentCuffPressure)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_intermediateCuffPressureCurrentCuffPressure_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureCurrentCuffPressure)).check(matches(withText(String.valueOf(intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg.getSfloat()))));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureCurrentCuffPressure)).check(matches(withText(String.valueOf(intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg.getSfloat()))));
    }

    @Test
    public void test_recreate_intermediateCuffPressureTimeStamp_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureTimeStamp)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureTimeStamp)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_intermediateCuffPressureTimeStamp_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_TIME_STAMP_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureTimeStamp)).check(matches(withText(mFakeDeviceSettingRepository.getDateTimeString(intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds))));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureTimeStamp)).check(matches(withText(mFakeDeviceSettingRepository.getDateTimeString(intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds))));
    }

    @Test
    public void test_recreate_intermediateCuffPressurePulseRate_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressurePulseRate)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressurePulseRate)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_intermediateCuffPressurePulseRate_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_PULSE_RATE_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressurePulseRate)).check(matches(withText(String.valueOf(intermediateCuffPressurePulseRate.getSfloat()))));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressurePulseRate)).check(matches(withText(String.valueOf(intermediateCuffPressurePulseRate.getSfloat()))));
    }

    @Test
    public void test_recreate_intermediateCuffPressureUserId_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureUserId)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureUserId)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_intermediateCuffPressureUserId_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_USER_ID_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureUserId)).check(matches(withText(String.valueOf(intermediateCuffPressureUserId))));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureUserId)).check(matches(withText(String.valueOf(intermediateCuffPressureUserId))));
    }

    @Test
    public void test_recreate_intermediateCuffPressureMeasurementStatus_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.intermediateCuffPressureMeasurementStatus)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureMeasurementStatus)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_intermediateCuffPressureMeasurementStatus_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_MEASUREMENT_STATUS_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) intermediateCuffPressureMeasurementStatusFlags,
                (byte) (intermediateCuffPressureMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_NOTIFY
                , 0
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , intermediateCuffPressure.getBytes()
                , -1);
        mViewModel.setIntermediateCuffPressureData(Utils.parcelableToByteArray(intermediateCuffPressureCharacteristicData));

        onView(withId(R.id.intermediateCuffPressureMeasurementStatus)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureMeasurementStatusFlags, 4))));

        mScenario.recreate();

        onView(withId(R.id.intermediateCuffPressureMeasurementStatus)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureMeasurementStatusFlags, 4))));
    }

    @Test
    public void test_recreate_bloodPressureFeatureCardView_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bloodPressureFeatureCardView)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureFeatureCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_bloodPressureFeatureCardView_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        mViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());

        onView(withId(R.id.bloodPressureFeatureCardView)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureFeatureCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_bloodPressureFeature_00001() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        onView(withId(R.id.bloodPressureFeature)).check(matches(withText("")));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureFeature)).check(matches(withText("")));
    }

    @Test
    public void test_recreate_bloodPressureFeature_00002() {
        Intent intent = new Intent(mContext, BloodPressureServiceSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = (FakeBloodPressureServiceSettingViewModel) new ViewModelProvider(activity).get(BloodPressureServiceSettingViewModel.class));

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        mViewModel.setBloodPressureFeatureData(Utils.parcelableToByteArray(bloodPressureFeatureCharacteristicData));

        onView(withId(R.id.bloodPressureFeature)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureFeature.getBytes(), 0), 4))));

        mScenario.recreate();

        onView(withId(R.id.bloodPressureFeature)).check(matches(withText(mFakeDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureFeature.getBytes(), 0), 4))));
    }

}