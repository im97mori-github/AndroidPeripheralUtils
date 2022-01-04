package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.intent.Intents.intended;
import static androidx.test.espresso.intent.Intents.intending;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasComponent;
import static androidx.test.espresso.intent.matcher.IntentMatchers.hasExtra;
import static androidx.test.espresso.matcher.ViewMatchers.hasDescendant;
import static androidx.test.espresso.matcher.ViewMatchers.isChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isNotChecked;
import static androidx.test.espresso.matcher.ViewMatchers.withEffectiveVisibility;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.test.TestUtils.createHiltActivity;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;
import static org.mockito.Mockito.mockStatic;

import android.app.Activity;
import android.app.Instrumentation;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.core.app.ApplicationProvider;
import androidx.test.espresso.intent.Intents;
import androidx.test.espresso.matcher.ViewMatchers;

import org.im97mori.ble.MockData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.test.HiltTestActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.FakeDeviceSettingViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.u180a.DeviceInformationServiceSettingActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.u1810.BloodPressureServiceSettingActivity;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
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
public class BloodPressureProfileFragmentTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    @ApplicationContext
    Context mContext;

    private ActivityScenario<HiltTestActivity> mScenario;

    private static MockedStatic<AutoDisposeViewModelProvider> mockedStatic;

    private FakeBloodPressureProfileViewModel mFakeBloodPressureProfileViewModel;

    @BeforeClass
    public static void setUpClass() {
        mockedStatic = mockStatic(AutoDisposeViewModelProvider.class);
        mockedStatic.when(() -> AutoDisposeViewModelProvider.getViewModelClass(DeviceSettingViewModel.class))
                .thenReturn(FakeDeviceSettingViewModel.class);
        mockedStatic.when(() -> AutoDisposeViewModelProvider.getViewModelClass(BloodPressureProfileViewModel.class))
                .thenReturn(FakeBloodPressureProfileViewModel.class);
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
    public void test_bloodPressureServiceCardView_checked_00001() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow());

        onView(withId(R.id.bloodPressureServiceCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_bloodPressureServiceCardView_checked_00002() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> {
            activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow();
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeBloodPressureProfileViewModel.setBlsDataJson("a");

        onView(withId(R.id.bloodPressureServiceCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_bloodPressureServiceCardView_title_00001() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow());

        onView(withId(R.id.bloodPressureServiceCardView)).check(matches(hasDescendant(withText(R.string.blood_pressure_service))));
    }

    @Test
    public void test_bloodPressureServiceSettingButton_00001() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow());

        onView(withId(R.id.bloodPressureServiceSettingButton)).perform(click());

        intended(hasComponent(new ComponentName(mContext, BloodPressureServiceSettingActivity.class)));
        intended(hasExtra(BLOOD_PRESSURE_SERVICE.toString(), null));
    }

    @Test
    public void test_bloodPressureServiceSettingButton_00002() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> {
            activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow();
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        String original = "a";
        mFakeBloodPressureProfileViewModel.setBlsDataJson(original);

        onView(withId(R.id.bloodPressureServiceSettingButton)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureServiceSettingActivity.class)));
        intended(hasExtra(BLOOD_PRESSURE_SERVICE.toString(), original));
    }

    @Test
    public void test_isDeviceInformationServiceSupported_00001() {
        AtomicReference<Boolean> result = new AtomicReference<>();

        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> {
            activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow();
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
            mFakeBloodPressureProfileViewModel.observeIsDisSupported(activity, result::set);
        });

        mFakeBloodPressureProfileViewModel.observeSetup(new MockData(), () -> {
        }, throwable -> {
        });

        assertFalse(result.get());
        onView(withId(R.id.isDeviceInformationServiceSupported)).check(matches(isNotChecked()));
        onView(withId(R.id.isDeviceInformationServiceSupported)).perform(click());
        onView(withId(R.id.isDeviceInformationServiceSupported)).check(matches(isChecked()));
        assertTrue(result.get());
    }

    @Test
    public void test_deviceInformationServiceCardView_visibility_00001() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow());

        onView(withId(R.id.deviceInformationServiceCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
    }

    @Test
    public void test_deviceInformationServiceCardView_visibility_00002() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow());

        onView(withId(R.id.isDeviceInformationServiceSupported)).perform(click());

        onView(withId(R.id.deviceInformationServiceCardView)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_deviceInformationServiceCardView_checked_00001() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow());

        onView(withId(R.id.deviceInformationServiceCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_deviceInformationServiceCardView_checked_00002() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> {
            activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow();
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeBloodPressureProfileViewModel.setDisDataJson("");

        onView(withId(R.id.deviceInformationServiceCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_deviceInformationServiceCardView_title_00001() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow());

        onView(withId(R.id.deviceInformationServiceCardView)).check(matches(hasDescendant(withText(R.string.device_information_service))));
    }

    @Test
    public void test_deviceInformationServiceSettingButton_00001() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow());

        onView(withId(R.id.isDeviceInformationServiceSupported)).perform(click());
        onView(withId(R.id.deviceInformationServiceSettingButton)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceInformationServiceSettingActivity.class)));
        intended(hasExtra(DEVICE_INFORMATION_SERVICE.toString(), null));
    }

    @Test
    public void test_deviceInformationServiceSettingButton_00002() {
        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> {
            activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow();
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        String original = "a";
        mFakeBloodPressureProfileViewModel.setDisDataJson(original);

        onView(withId(R.id.isDeviceInformationServiceSupported)).perform(click());
        onView(withId(R.id.deviceInformationServiceSettingButton)).perform(click());

        intended(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceInformationServiceSettingActivity.class)));
        intended(hasExtra(DEVICE_INFORMATION_SERVICE.toString(), original));
    }

    @Test
    public void test_activity_result_00001() {
        Intent resultData = new Intent();
        String after = "b";
        resultData.putExtra(BLOOD_PRESSURE_SERVICE.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), BloodPressureServiceSettingActivity.class))).respondWith(result);

        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> {
            activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow();
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        String before = "a";
        mFakeBloodPressureProfileViewModel.setBlsDataJson(before);

        onView(withId(R.id.bloodPressureServiceSettingButton)).perform(click());

        assertEquals(after, mFakeBloodPressureProfileViewModel.getBlsDataJson());
    }

    @Test
    public void test_activity_result_00002() {
        Intent resultData = new Intent();
        String after = "b";
        resultData.putExtra(DEVICE_INFORMATION_SERVICE.toString(), after);
        Instrumentation.ActivityResult result = new Instrumentation.ActivityResult(Activity.RESULT_OK, resultData);
        intending(hasComponent(new ComponentName(ApplicationProvider.getApplicationContext(), DeviceInformationServiceSettingActivity.class))).respondWith(result);

        mScenario = createHiltActivity();
        BloodPressureProfileFragment bloodPressureProfileFragment = new BloodPressureProfileFragment();
        mScenario.onActivity(activity -> {
            activity.getSupportFragmentManager().beginTransaction().add(R.id.fragmentContainer, bloodPressureProfileFragment).commitNow();
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        String before = "a";
        mFakeBloodPressureProfileViewModel.setDisDataJson(before);

        onView(withId(R.id.isDeviceInformationServiceSupported)).perform(click());
        onView(withId(R.id.deviceInformationServiceSettingButton)).perform(click());

        onView(withId(R.id.bloodPressureServiceSettingButton)).perform(click());

        assertEquals(after, mFakeBloodPressureProfileViewModel.getDisDataJson());
    }

}