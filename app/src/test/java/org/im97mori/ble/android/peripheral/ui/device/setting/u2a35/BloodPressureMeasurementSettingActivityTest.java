package org.im97mori.ble.android.peripheral.ui.device.setting.u2a35;

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
import static androidx.test.espresso.matcher.ViewMatchers.withHint;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT;
import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mockStatic;

import android.app.Activity;
import android.app.Instrumentation;
import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattDescriptor;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.text.TextUtils;
import android.widget.AutoCompleteTextView;
import android.widget.RadioGroup;
import android.widget.TextView;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.espresso.Espresso;
import androidx.test.espresso.intent.Intents;
import androidx.test.espresso.matcher.RootMatchers;
import androidx.test.espresso.matcher.ViewMatchers;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputLayout;
import com.google.gson.Gson;

import junit.framework.TestCase;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.u2902.ClientCharacteristicConfigurationSettingActivity;
import org.im97mori.ble.android.peripheral.utils.MockitoViewModelProvider;
import org.im97mori.ble.characteristic.core.IEEE_11073_20601_SFLOAT;
import org.im97mori.ble.characteristic.u2a35.BloodPressureMeasurement;
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

import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;

@SuppressWarnings("ConstantConditions")
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class BloodPressureMeasurementSettingActivityTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<BloodPressureMeasurementSettingActivity> mScenario;

    private FakeBloodPressureMeasurementSettingViewModel mViewModel;

    private static MockedStatic<MockitoViewModelProvider> mockedStatic;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    Gson mGson;

    @Inject
    FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    @BeforeClass
    public static void setUpClass() {
        mockedStatic = mockStatic(MockitoViewModelProvider.class);
        mockedStatic.when(() -> MockitoViewModelProvider.getViewModelClass(BloodPressureMeasurementSettingViewModel.class))
                .thenReturn(FakeBloodPressureMeasurementSettingViewModel.class);
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
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));
        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.blood_pressure_measurement))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupProcessor.onNext("test_root_container_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_menu_save_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isNotEnabled()));
    }

    @Test
    public void test_menu_save_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_menu_save_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_menu_save_00003");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).perform(click());

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData();
        bloodPressureMeasurementCharacteristicData.uuid = BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
        bloodPressureMeasurementCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_INDICATE;
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
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        bloodPressureMeasurementCharacteristicData.descriptorDataList.add(clientCharacteristicConfigurationDescriptorData);

        String json = mGson.toJson(bloodPressureMeasurementCharacteristicData);
        Intent original = new Intent();
        original.putExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), json);
        mViewModel.mObserveSaveProcessor.onNext(original);

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_OK, activityResult.getResultCode());
        Intent resultData = activityResult.getResultData();
        assertNotNull(resultData);
        assertEquals(json, resultData.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString()));
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        TestCase.assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_activity_result_00001() {
        Intent resultData = new Intent();
        String after = "b";
        resultData.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.setClientCharacteristicConfigurationDescriptorJson(null);

        mScenario.onActivity(activity -> activity.findViewById(R.id.clientCharacteristicConfigurationSettingButton).performClick());
        Espresso.onIdle();

        assertEquals(after, mViewModel.getClientCharacteristicConfigurationDescriptorJson());
    }

    @Test
    public void test_activity_result_00002() {
        Intent resultData = new Intent();
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_CANCELED, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class))).respondWith(result);

        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        String before = "a";
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(before);

        mScenario.onActivity(activity -> activity.findViewById(R.id.clientCharacteristicConfigurationSettingButton).performClick());
        Espresso.onIdle();

        assertNull(mViewModel.getClientCharacteristicConfigurationDescriptorJson());
    }

    @Test
    public void test_unitRadioGroup_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsMmhgConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_unitRadioGroup_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            RadioGroup radioGroup = activity.findViewById(R.id.unitRadioGroup);
            radioGroup.check(R.id.mmhgRadioButton);
        });

        assertTrue(result.get());
    }

    @Test
    public void test_unitRadioGroup_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsMmhgConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_unitRadioGroup_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            RadioGroup radioGroup = activity.findViewById(R.id.unitRadioGroup);
            radioGroup.check(R.id.mmhgRadioButton);
            radioGroup.check(R.id.kpaRadioButton);
        });

        assertFalse(result.get());
    }

    @Test
    public void test_mmhgRadioButton_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.mmhgRadioButton)).check(matches(withText(R.string.mmhg)));
    }

    @Test
    public void test_kpaRadioButton_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.kpaRadioButton)).check(matches(withText(R.string.kpa)));
    }

    @Test
    public void test_systolic_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.systolic);
            assertEquals(mContext.getString(R.string.sfloat), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_systolic_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.systolicEdit)).check(matches(withHint(R.string.systolic)));
    }

    @Test
    public void test_systolic_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.systolic)).getError())));
    }

    @Test
    public void test_systolic_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_systolic_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.systolic)).getError()).toString()));
    }

    @Test
    public void test_updateSystolic_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateSystolicConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateSystolic_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.systolicEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_diastolic_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.diastolic);
            assertEquals(mContext.getString(R.string.sfloat), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_diastolic_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.diastolicEdit)).check(matches(withHint(R.string.diastolic)));
    }

    @Test
    public void test_diastolic_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.diastolic)).getError())));
    }

    @Test
    public void test_diastolic_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_diastolic_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.diastolic)).getError()).toString()));
    }

    @Test
    public void test_updateDiastolic_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateDiastolicConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateDiastolic_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.diastolicEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_meanArterialPressure_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.meanArterialPressure);
            assertEquals(mContext.getString(R.string.sfloat), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_meanArterialPressure_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.meanArterialPressureEdit)).check(matches(withHint(R.string.mean_arterial_pressure)));
    }

    @Test
    public void test_meanArterialPressure_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.meanArterialPressure)).getError())));
    }

    @Test
    public void test_meanArterialPressure_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_meanArterialPressure_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.meanArterialPressure)).getError()).toString()));
    }

    @Test
    public void test_updateMeanArterialPressure_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateMeanArterialPressureConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateMeanArterialPressure_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.meanArterialPressureEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_isTimeStampSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.isTimeStampSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isTimeStampSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isTimeStampSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isTimeStampSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isTimeStampSupported_00003() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isTimeStampSupported_00003");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isTimeStampSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_updateIsTimeStampSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsTimeStampSupportedConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateIsTimeStampSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> activity.findViewById(R.id.isTimeStampSupported).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_timeStampYear_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampYear_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampYear_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampYear_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampYear)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampYear_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampYear);
            assertEquals(mContext.getString(R.string.time_stamp_year_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampYear_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.timeStampYearEdit)).check(matches(withHint(R.string.year)));
    }

    @Test
    public void test_timeStampYear_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.timeStampYear)).getError())));
    }

    @Test
    public void test_timeStampYear_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampYear_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.timeStampYear)).getError()).toString()));
    }

    @Test
    public void test_updateTimeStampYear_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampYearConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateTimeStampYear_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.timeStampYearEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_timeStampMonth_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampMonth_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampMonth_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampMonth_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampMonth)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampMonth_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampMonth);
            assertEquals(mContext.getString(R.string.time_stamp_month_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampMonth_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.timeStampMonthEdit)).check(matches(withHint(R.string.month)));
    }

    @Test
    public void test_updateTimeStampMonth_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampMonthConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateTimeStampMonth_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampMonthEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeMonthList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_timeStampDay_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampDay_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampDay_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampDay_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampDay)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampDay_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampDay);
            assertEquals(mContext.getString(R.string.time_stamp_day_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampDay_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.timeStampDayEdit)).check(matches(withHint(R.string.day)));
    }

    @Test
    public void test_updateTimeStampDay_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampDayConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateTimeStampDay_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampDayEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeDayList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_timeStampHours_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampHours_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampHours_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampHours_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampHours)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampHours_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampHours);
            assertEquals(mContext.getString(R.string.time_stamp_hours_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampHours_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.timeStampHoursEdit)).check(matches(withHint(R.string.hours)));
    }

    @Test
    public void test_updateTimeStampHours_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampHoursConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateTimeStampHours_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampHoursEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeHoursList().get(original))).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_timeStampMinutes_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampMinutes_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampMinutes_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampMinutes_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampMinutes)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampMinutes_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampMinutes);
            assertEquals(mContext.getString(R.string.time_stamp_minutes_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampMinutes_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.timeStampMinutesEdit)).check(matches(withHint(R.string.minutes)));
    }

    @Test
    public void test_updateTimeStampMinutes_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampMinutesConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateTimeStampMinutes_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampMinutesEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeMinutesList().get(original))).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_timeStampSeconds_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampSeconds_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_timeStampSeconds_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_timeStampSeconds_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.timeStampSeconds)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_timeStampSeconds_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.timeStampSeconds);
            assertEquals(mContext.getString(R.string.time_stamp_seconds_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_timeStampSeconds_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.timeStampSecondsEdit)).check(matches(withHint(R.string.seconds)));
    }

    @Test
    public void test_updateTimeStampSeconds_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateTimeStampSecondsConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateTimeStampSeconds_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.timeStampSecondsEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideDateTimeMinutesList().get(original))).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_isPulseRateSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsPulseRateSupportedConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isPulseRateSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> activity.findViewById(R.id.isPulseRateSupported).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_isPulseRateSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isPulseRateSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isPulseRateSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isPulseRateSupported_00003() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isPulseRateSupported_00003");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isPulseRateSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_pulseRate_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_pulseRate_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_pulseRate_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_pulseRate_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.pulseRate)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_pulseRate_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.pulseRate);
            assertEquals(mContext.getString(R.string.sfloat), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_pulseRate_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.pulseRateEdit)).check(matches(withHint(R.string.pulse_rate)));
    }

    @Test
    public void test_pulseRate_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.pulseRate)).getError())));
    }

    @Test
    public void test_pulseRate_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_pulseRate_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.pulseRate)).getError()).toString()));
    }

    @Test
    public void test_updatePulseRate_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdatePulseRateConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updatePulseRate_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.pulseRateEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_isUserIdSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsUserIdSupportedConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isUserIdSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> activity.findViewById(R.id.isUserIdSupported).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_isUserIdSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isUserIdSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isUserIdSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isUserIdSupported_00003() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isUserIdSupported_00003");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isUserIdSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_userId_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_userId_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_userId_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_userId_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.userId)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_userId_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.userId);
            assertEquals(mContext.getString(R.string.user_id_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_userId_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.userIdEdit)).check(matches(withHint(R.string.user_id)));
    }

    @Test
    public void test_userId_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.userId)).getError())));
    }

    @Test
    public void test_userId_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_userId_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.userId)).getError()).toString()));
    }

    @Test
    public void test_updateUserId_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateUserIdConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateUserId_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.userIdEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_isMeasurementStatusSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsMeasurementStatusSupportedConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isMeasurementStatusSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> activity.findViewById(R.id.isMeasurementStatusSupported).performClick());

        assertTrue(result.get());
    }

    @Test
    public void test_isMeasurementStatusSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isMeasurementStatusSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMeasurementStatusSupported)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isMeasurementStatusSupported_00003() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isMeasurementStatusSupported_00003");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMeasurementStatusSupported)).check(matches(isChecked()));
    }

    @Test
    public void test_bodyMovementDetection_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_bodyMovementDetection_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_bodyMovementDetection_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_bodyMovementDetection_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.bodyMovementDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_bodyMovementDetection_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.bodyMovementDetectionEdit)).check(matches(withHint(R.string.body_movement_detection)));
    }

    @Test
    public void test_bodyMovementDetection_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateBodyMovementDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_bodyMovementDetection_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.bodyMovementDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideBodyMovementDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_cuffFitDetection_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_cuffFitDetection_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_cuffFitDetection_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_cuffFitDetection_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.cuffFitDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_cuffFitDetection_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.cuffFitDetectionEdit)).check(matches(withHint(R.string.cuff_fit_detection)));
    }

    @Test
    public void test_cuffFitDetection_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateCuffFitDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_cuffFitDetection_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.cuffFitDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideCuffFitDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_irregularPulseDetection_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_irregularPulseDetection_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_irregularPulseDetection_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_irregularPulseDetection_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.irregularPulseDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_irregularPulseDetection_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.irregularPulseDetectionEdit)).check(matches(withHint(R.string.irregular_pulse_detection)));
    }

    @Test
    public void test_irregularPulseDetection_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateIrregularPulseDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_irregularPulseDetection_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.irregularPulseDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideIrregularPulseDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_pulseRateRangeDetection_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_pulseRateRangeDetection_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_pulseRateRangeDetection_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_irregularPulseDetection_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.pulseRateRangeDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_pulseRateRangeDetection_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.pulseRateRangeDetectionEdit)).check(matches(withHint(R.string.pulse_rate_range_detection)));
    }

    @Test
    public void test_pulseRateRangeDetection_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdatePulseRateRangeDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_pulseRateRangeDetection_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.pulseRateRangeDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.providePulseRateRangeDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_measurementPositionDetection_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_measurementPositionDetection_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_measurementPositionDetection_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_measurementPositionDetection_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.measurementPositionDetection)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_measurementPositionDetection_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.measurementPositionDetectionEdit)).check(matches(withHint(R.string.measurement_position_detection)));
    }

    @Test
    public void test_measurementPositionDetection_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        int original = 1;
        AtomicReference<Integer> result = new AtomicReference<>();
        mViewModel.mUpdateMeasurementPositionDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_measurementPositionDetection_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            AutoCompleteTextView textView = activity.findViewById(R.id.measurementPositionDetectionEdit);
            textView.showDropDown();
        });

        onView(withText(mViewModel.provideMeasurementPositionDetectionList().get(original).second)).inRoot(RootMatchers.isPlatformPopup()).perform(click());

        assertEquals(original, result.get().intValue());
    }

    @Test
    public void test_clientCharacteristicConfigurationCardView_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mObserveSetupProcessor.onNext("test_clientCharacteristicConfigurationCardView_00001");
        mViewModel.mObserveSetupProcessor.onComplete();

        onView(withId(R.id.clientCharacteristicConfigurationCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_clientCharacteristicConfigurationCardView_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mObserveSetupProcessor.onNext("test_clientCharacteristicConfigurationCardView_00002");
        mViewModel.mObserveSetupProcessor.onComplete();

        onView(withId(R.id.clientCharacteristicConfigurationCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_clientCharacteristicConfigurationCardView_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.clientCharacteristicConfigurationCardViewTitle)).check(matches(withText(R.string.client_characteristic_configuration)));
    }

    @Test
    public void test_clientCharacteristicConfiguration_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.clientCharacteristicConfiguration)).check(matches(withText("")));
    }

    @Test
    public void test_clientCharacteristicConfiguration_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE;
        String originalClientCharacteristicConfigurationDescriptorData = mGson.toJson(clientCharacteristicConfigurationDescriptorData);
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(originalClientCharacteristicConfigurationDescriptorData);

        onView(withId(R.id.clientCharacteristicConfiguration))
                .check(matches(withText(mFakeDeviceSettingRepository.getIndicationsDisabledString())));
    }

    @Test
    public void test_clientCharacteristicConfiguration_00003() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        String originalClientCharacteristicConfigurationDescriptorData = mGson.toJson(clientCharacteristicConfigurationDescriptorData);
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(originalClientCharacteristicConfigurationDescriptorData);

        onView(withId(R.id.clientCharacteristicConfiguration))
                .check(matches(withText(mFakeDeviceSettingRepository.getIndicationsEnabledString())));
    }

    @Test
    public void test_clientCharacteristicConfigurationSettingButton_text_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.clientCharacteristicConfigurationSettingButton)).check(matches(withText(R.string.setting)));
    }

    @Test
    public void test_clientCharacteristicConfigurationSettingButton_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mObserveSetupProcessor.onNext("test_clientCharacteristicConfigurationSettingButton_00001");
        mViewModel.mObserveSetupProcessor.onComplete();

        mScenario.onActivity(activity -> activity.findViewById(R.id.clientCharacteristicConfigurationSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class)));
        intended(hasExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), (String) null));
    }

    @Test
    public void test_clientCharacteristicConfigurationSettingButton_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mObserveSetupProcessor.onNext("test_clientCharacteristicConfigurationSettingButton_00002");
        mViewModel.mObserveSetupProcessor.onComplete();

        mScenario.onActivity(activity -> activity.findViewById(R.id.clientCharacteristicConfigurationSettingButton).performClick());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), ClientCharacteristicConfigurationSettingActivity.class)));
        intended(hasExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), "a"));
    }

    @Test
    public void test_indicationCount_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.indicationCount);
            assertEquals(mContext.getString(R.string.notification_count_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_indicationCount_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        onView(withId(R.id.indicationCountEdit)).check(matches(withHint(R.string.indication_count)));
    }

    @Test
    public void test_indicationCount_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.indicationCount)).getError())));
    }

    @Test
    public void test_indicationCount_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_indicationCount_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.indicationCount)).getError()).toString()));
    }

    @Test
    public void test_updateIndicationCount_00001() {
        Intent intent = new Intent(mContext, BloodPressureMeasurementSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureMeasurementSettingViewModel.class));

        String original = "a";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateIndicationCountConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateIndicationCount_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.indicationCountEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

}