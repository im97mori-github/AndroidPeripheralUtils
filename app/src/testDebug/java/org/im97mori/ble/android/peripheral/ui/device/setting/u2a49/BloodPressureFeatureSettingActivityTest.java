package org.im97mori.ble.android.peripheral.ui.device.setting.u2a49;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBack;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.hasDescendant;
import static androidx.test.espresso.matcher.ViewMatchers.isChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isEnabled;
import static androidx.test.espresso.matcher.ViewMatchers.isNotChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isNotEnabled;
import static androidx.test.espresso.matcher.ViewMatchers.withEffectiveVisibility;
import static androidx.test.espresso.matcher.ViewMatchers.withHint;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mockStatic;

import android.app.Activity;
import android.app.Instrumentation;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Context;
import android.content.Intent;
import android.os.Build;
import android.text.TextUtils;
import android.widget.TextView;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.espresso.matcher.ViewMatchers;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputLayout;
import com.google.gson.Gson;

import junit.framework.TestCase;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;
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

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class BloodPressureFeatureSettingActivityTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<BloodPressureFeatureSettingActivity> mScenario;

    private FakeBloodPressureFeatureViewModel mViewModel;

    private static MockedStatic<AutoDisposeViewModelProvider> mockedStatic;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    Gson mGson;

    @BeforeClass
    public static void setUpClass() {
        mockedStatic = mockStatic(AutoDisposeViewModelProvider.class);
        mockedStatic.when(() -> AutoDisposeViewModelProvider.getViewModelClass(BloodPressureFeatureSettingViewModel.class))
                .thenReturn(FakeBloodPressureFeatureViewModel.class);
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
    }

    @After
    public void tearDown() {
        mScenario.close();
    }

    @Test
    public void test_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));
        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.blood_pressure_feature))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mViewModel.mObserveSetupProcessor.onNext("test_root_container_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isErrorResponse_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        onView(withId(R.id.isErrorResponse)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isErrorResponse_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isErrorResponse_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isErrorResponse)).check(matches(isNotChecked()));
    }

    @Test
    public void test_isErrorResponse_00003() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isErrorResponse_00003");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isErrorResponse)).check(matches(isChecked()));
    }

    @Test
    public void test_updateIsErrorResponse_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIsErrorResponseConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateIsErrorResponse_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isErrorResponse)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_responseCode_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_responseCode_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_responseCode_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_responseCode_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.responseCode)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_responseCode_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseCode);
            assertEquals(mContext.getString(R.string.error_response_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_responseCode_prefixText_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseCode);
            assertEquals(mContext.getString(R.string.hexadecimal), textInputLayout.getPrefixText());
        });
    }

    @Test
    public void test_responseCode_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);

        onView(withId(R.id.responseCodeEdit)).check(matches(withHint(R.string.response_code)));
    }

    @Test
    public void test_responseCode_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_responseCode_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.responseCodeEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_responseCode_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError())));
    }

    @Test
    public void test_responseCode_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_responseCode_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseCode)).getError()).toString()));
    }

    @Test
    public void test_updateResponseCode_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        String original = "1";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateResponseCodeConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateResponseCode_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.responseCodeEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_responseDelay_helper_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            TextInputLayout textInputLayout = activity.findViewById(R.id.responseDelay);
            assertEquals(mContext.getString(R.string.response_delay_helper_text), textInputLayout.getHelperText());
        });
    }

    @Test
    public void test_responseDelay_hint_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);

        onView(withId(R.id.responseDelayEdit)).check(matches(withHint(R.string.response_delay)));
    }

    @Test
    public void test_responseDelay_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_responseDelay_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.responseDelayEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_responseDelay_error_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError())));
    }

    @Test
    public void test_responseDelay_error_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_responseDelay_error_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.responseDelay)).getError()).toString()));
    }

    @Test
    public void test_updateResponseDelay_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        String original = "1";
        AtomicReference<String> result = new AtomicReference<>();
        mViewModel.mUpdateResponseDelayConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_updateResponseDelay_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.responseDelayEdit);
            textView.setText(original);
        });

        assertEquals(original, result.get());
    }

    @Test
    public void test_menu_save_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isNotEnabled()));
    }

    @Test
    public void test_menu_save_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_menu_save_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_menu_save_00003");
        mViewModel.mObserveSetupProcessor.onComplete();
        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).perform(click());

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        characteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        characteristicData.data = new BloodPressureFeature(false
                , false
                , false
                , false
                , false
                , false
                , false
                , false
                , false).getBytes();
        String json = mGson.toJson(characteristicData);
        Intent original = new Intent();
        original.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), json);
        mViewModel.mObserveSaveProcessor.onNext(original);

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_OK, activityResult.getResultCode());
        Intent resultData = activityResult.getResultData();
        assertNotNull(resultData);
        assertEquals(json, resultData.getStringExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString()));
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        TestCase.assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_isBodyMovementDetectionSupported_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isBodyMovementDetectionSupported_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isBodyMovementDetectionSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isBodyMovementDetectionSupported_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isBodyMovementDetectionSupported_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isBodyMovementDetectionSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_isBodyMovementDetectionSupported_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        onView(withId(R.id.isBodyMovementDetectionSupported)).check(matches(withText(R.string.body_movement_detection_support)));
    }

    @Test
    public void test_isBodyMovementDetectionSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateBodyMovementDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isBodyMovementDetectionSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isBodyMovementDetectionSupported)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_isBodyMovementDetectionSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateBodyMovementDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isBodyMovementDetectionSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isBodyMovementDetectionSupported)).perform(click());

        assertFalse(result.get());
    }

    @Test
    public void test_isCuffFitDetectionSupportSupported_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isCuffFitDetectionSupportSupported_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isCuffFitDetectionSupportSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isCuffFitDetectionSupportSupported_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isCuffFitDetectionSupportSupported_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isCuffFitDetectionSupportSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_isCuffFitDetectionSupportSupported_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        onView(withId(R.id.isCuffFitDetectionSupportSupported)).check(matches(withText(R.string.cuff_fit_detection_support)));
    }

    @Test
    public void test_isCuffFitDetectionSupportSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateCuffFitDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isCuffFitDetectionSupportSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isCuffFitDetectionSupportSupported)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_isCuffFitDetectionSupportSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateCuffFitDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isCuffFitDetectionSupportSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isCuffFitDetectionSupportSupported)).perform(click());

        assertFalse(result.get());
    }

    @Test
    public void test_isIrregularPulseDetectionSupported_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isIrregularPulseDetectionSupported_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isIrregularPulseDetectionSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isIrregularPulseDetectionSupported_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isIrregularPulseDetectionSupported_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isIrregularPulseDetectionSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_isIrregularPulseDetectionSupported_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        onView(withId(R.id.isIrregularPulseDetectionSupported)).check(matches(withText(R.string.irregular_pulse_detection_support)));
    }

    @Test
    public void test_isIrregularPulseDetectionSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIrregularPulseDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isIrregularPulseDetectionSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isIrregularPulseDetectionSupported)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_isIrregularPulseDetectionSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateIrregularPulseDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isIrregularPulseDetectionSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isIrregularPulseDetectionSupported)).perform(click());

        assertFalse(result.get());
    }

    @Test
    public void test_isPulseRateRangeDetectionSupported_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isPulseRateRangeDetectionSupported_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isPulseRateRangeDetectionSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isPulseRateRangeDetectionSupported_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isPulseRateRangeDetectionSupported_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isPulseRateRangeDetectionSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_isPulseRateRangeDetectionSupported_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        onView(withId(R.id.isPulseRateRangeDetectionSupported)).check(matches(withText(R.string.pulse_rate_range_detection_support)));
    }

    @Test
    public void test_isPulseRateRangeDetectionSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdatePulseRateRangeDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isPulseRateRangeDetectionSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isPulseRateRangeDetectionSupported)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_isPulseRateRangeDetectionSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdatePulseRateRangeDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isPulseRateRangeDetectionSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isPulseRateRangeDetectionSupported)).perform(click());

        assertFalse(result.get());
    }

    @Test
    public void test_isMeasurementPositionDetectionSupported_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isMeasurementPositionDetectionSupported_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMeasurementPositionDetectionSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isMeasurementPositionDetectionSupported_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isMeasurementPositionDetectionSupported_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMeasurementPositionDetectionSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_isMeasurementPositionDetectionSupported_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        onView(withId(R.id.isMeasurementPositionDetectionSupported)).check(matches(withText(R.string.measurement_position_detection_support)));
    }

    @Test
    public void test_isMeasurementPositionDetectionSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateMeasurementPositionDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isMeasurementPositionDetectionSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMeasurementPositionDetectionSupported)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_isMeasurementPositionDetectionSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateMeasurementPositionDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isMeasurementPositionDetectionSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMeasurementPositionDetectionSupported)).perform(click());

        assertFalse(result.get());
    }

    @Test
    public void test_isMultipleBondSupported_visibility_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isMultipleBondSupported_visibility_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMultipleBondSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_isMultipleBondSupported_visibility_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        mViewModel.mObserveSetupProcessor.onNext("test_isMultipleBondSupported_visibility_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMultipleBondSupported)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_isMultipleBondSupported_title_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        onView(withId(R.id.isMultipleBondSupported)).check(matches(withText(R.string.multiple_bonds_support)));
    }

    @Test
    public void test_isMultipleBondSupported_00001() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateMultipleBondDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isMultipleBondSupported_00001");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMultipleBondSupported)).perform(click());

        assertTrue(result.get());
    }

    @Test
    public void test_isMultipleBondSupported_00002() {
        Intent intent = new Intent(mContext, BloodPressureFeatureSettingActivity.class);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeBloodPressureFeatureViewModel.class));

        AtomicReference<Boolean> result = new AtomicReference<>();
        mViewModel.mUpdateMultipleBondDetectionConsumer = result::set;
        mViewModel.mObserveSetupProcessor.onNext("test_isMultipleBondSupported_00002");
        mViewModel.mObserveSetupProcessor.onComplete();
        onView(withId(R.id.isMultipleBondSupported)).perform(click());

        assertFalse(result.get());
    }

}